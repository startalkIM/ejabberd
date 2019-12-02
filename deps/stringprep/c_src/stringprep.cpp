/*
 * Copyright (C) 2002-2016   ProcessOne
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 *
 */

#include <string.h>
#include <stdint.h>
#include <erl_nif.h>

#include "uni_data.c"
#include "uni_norm.c"

/* Hangul constants */
#define SBase 0xAC00
#define LBase 0x1100
#define VBase 0x1161
#define TBase 0x11A7
#define LCount 19
#define VCount 21
#define TCount 28
#define NCount (VCount * TCount)
#define SCount (LCount * NCount)

static int compose(int ch1, int ch2) {
  int info1, info2;

  if (LBase <= ch1 && ch1 < LBase + LCount &&
	  VBase <= ch2 && ch2 < VBase + VCount) {
	return SBase + ((ch1 - LBase) * VCount + (ch2 - VBase)) * TCount;
  }

  if (SBase <= ch1 && ch1 < SBase + SCount && ((ch1 - SBase) % TCount) == 0 &&
	  TBase <= ch2 && ch2 < TBase + TCount) {
	return ch1 + ch2 - TBase;
  }

  info1 = GetUniCharCompInfo(ch1);
  if (info1 != -1 && info1 & CompSingleMask) {
	if (!(info1 & CompSecondMask) &&
		ch2 == compFirstList[info1 & CompMask][0]) {
	  return compFirstList[info1 & CompMask][1];
	} else
	  return 0;
  }

  info2 = GetUniCharCompInfo(ch2);
  if (info2 != -1 && info2 & CompSingleMask) {
	if ((info2 & CompSecondMask) &&
		ch1 == compSecondList[info2 & CompMask][0]) {
	  return compSecondList[info2 & CompMask][1];
	} else
	  return 0;
  }

  if (info1 != -1 && info2 != -1 &&
	  !(info1 & CompSecondMask) && (info2 & CompSecondMask))
	return compBothList[info1][info2 & CompMask];
  else
	return 0;
}

template<class T, int N>
class MaybeStaticBuf {
 public:
  MaybeStaticBuf() : pos(0), size(N), len(0), buf(static_buf) { }
  ~MaybeStaticBuf() {
	if (buf != static_buf)
	  enif_free(buf);
  }
  T init(T ch) {
	len = 1;
	pos = 0;
	return buf[0] = ch;
  }
  T add(T ch) {
	if (len >= size) {
	  if (buf == static_buf) {
		T *old = buf;
		buf = (T *) enif_alloc(sizeof(T) * size * 2);
		if (!buf)
		  return -2;
		memcpy(buf, old, size * sizeof(T));
	  } else {
		buf = (T *) enif_realloc(buf, sizeof(T) * size * 2);
		if (!buf)
		  return -2;
	  }
	  size *= 2;
	}
	buf[len++] = ch;
	return ch;
  }

  void empty() {
	pos = len = 0;
  }

  void swap(int p1, int p2) {
	T ch = buf[p1];
	buf[p1] = buf[p2];
	buf[p2] = ch;
  }

  T operator[](int index) {
	return buf[index];
  }

  int pos;
  int size;
  int len;
 private:
  T static_buf[N];
  T *buf;
};

class UTF8DecoderStream {
 public:
  UTF8DecoderStream(ErlNifBinary *input) : input(input), pos(0) { };

  void reset() {
	pos = 0;
  }

  ErlNifBinary *getBinary() {
	return input;
  }

  int32_t getNext() {
	if (pos >= input->size)
	  return -1;
	unsigned char c = input->data[pos++];
	if (c <= 0x80) {
	  return c;
	} else if (c < 0xC0) {
	  return -2;
	} else if (c < 0xE0) {
	  if (pos < input->size && (input->data[pos] & 0xC0) == 0x80) {
		return ((c & 0x1F) << 6) | (input->data[pos++] & 0x3F);
	  }
	} else if (c < 0xF0) {
	  if (pos + 1 < input->size && (input->data[pos] & 0xC0) == 0x80 &&
		  (input->data[pos + 1] & 0xC0) == 0x80) {
		pos += 2;
		return ((c & 0x0F) << 12)
			| ((input->data[pos - 2] & 0x3F) << 6)
			| (input->data[pos - 1] & 0x3F);
	  }
	} else if (c < 0xF8) {
	  if (pos + 2 < input->size &&
		  (input->data[pos] & 0xC0) == 0x80 &&
		  (input->data[pos + 1] & 0xC0) == 0x80 &&
		  (input->data[pos + 2] & 0xC0) == 0x80) {
		int32_t wc = ((c & 0x07) << 18)
			| ((input->data[pos] & 0x3F) << 12)
			| ((input->data[pos + 1] & 0x3F) << 6)
			| (input->data[pos + 2] & 0x3F);
		pos += 3;
		if (wc <= 0x10FFFF)
		  return wc;
	  }
	}
	return -2;
  }

 private:
  ErlNifBinary *input;
  size_t pos;
};

class PreprocessStream {
 public:
  PreprocessStream(UTF8DecoderStream *source, bool toLower) :
	  source(source), buf(NULL), pos(0), len(0), toLower(toLower) {
  }

  int32_t getNext() {
	if (pos < len)
	  return buf[pos++];

	loop:
	int32_t ch = source->getNext();
	if (ch < 0)
	  return ch;
	int info = GetUniCharInfo(ch);

	if (!(info & B1Mask)) {
	  if (toLower) {
		if (!(info & MCMask)) {
		  return ch + GetDelta(info);
		} else {
		  buf = GetMC(info) + 1;
		  len = buf[-1];
		  pos = 1;
		  return buf[0];
		}
	  } else {
		return ch;
	  }
	} else
	  goto loop;
  }
 private:
  UTF8DecoderStream *source;
  int32_t *buf;
  int pos;
  int len;
  bool toLower;
};

class DecompositeStream {
 public:
  DecompositeStream(PreprocessStream *source) : source(source), pos(0), len(0) { }

  int32_t getNext() {
	if (pos < len)
	  return decompList[pos++];

	int32_t ch = source->getNext();

	if (ch < 0)
	  return ch;

	int info = GetUniCharDecompInfo(ch);
	if (info >= 0) {
	  pos = GetDecompShift(info);
	  len = pos + GetDecompLen(info);
	  return decompList[pos++];
	} else
	  return ch;
  }

 private:
  PreprocessStream *source;
  int pos;
  int len;
};

class CanonicalizeStream {
 public:
  CanonicalizeStream(DecompositeStream *source) : source(source), buf() {
  }

  int32_t getNext() {
	if (buf.pos < buf.len - 1)
	  return buf[buf.pos++];

	int32_t ch, ch2;
	if (buf.len > 0) {
	  ch = buf.init(buf[buf.len - 1]);
	} else {
	  ch = buf.init(source->getNext());

	  if (ch < 0)
		return ch;
	}

	buf.pos++;

	int last = GetUniCharCClass(ch);
	while ((ch2 = buf.add(source->getNext())) >= 0) {
	  int next = GetUniCharCClass(ch2);
	  if (next != 0 && last > next) {
		for (int j = buf.len - 2; j >= 0; j--) {
		  if (GetUniCharCClass(buf[j]) <= next)
			break;
		  buf.swap(j, j + 1);
		}
	  } else {
		return buf[0];
	  }
	}
	return buf[0];
  }

 private:
  DecompositeStream *source;
  MaybeStaticBuf<int32_t, 8> buf;
};

class ComposeStream {
 public:
  ComposeStream(CanonicalizeStream *source) : source(source), buf(), lastCh(-1) {
  }

  int32_t getNext() {
	int32_t ch, nch;

	if (buf.pos < buf.len)
	  return buf[buf.pos++];
	else
	  buf.empty();

	if (lastCh < 0) {
	  ch = source->getNext();
	  if (ch < 0)
		return ch;
	} else {
	  ch = lastCh;
	}

	int cclass1 = GetUniCharCClass(ch);
	while ((lastCh = source->getNext()) >= 0) {
	  int cclass2 = GetUniCharCClass(lastCh);
	  if ((cclass1 == 0 || cclass2 > cclass1) &&
		  (nch = compose(ch, lastCh))) {
		ch = nch;
	  } else if (cclass2 == 0) {
		return ch;
	  } else {
		buf.add(lastCh);
		cclass1 = cclass2;
	  }
	}

	if (lastCh >= -1)
	  return ch;
	else
	  return lastCh;
  }
 private:
  CanonicalizeStream *source;
  MaybeStaticBuf<int32_t, 8> buf;
  int32_t lastCh;
};

class PrepCheckStream {
 public:
  PrepCheckStream(ComposeStream *source, int32_t prohibit) :
	  source(source), prohibit(prohibit), first_ral(-1),
	  last_ral(0), have_ral(0), have_l(0) {
  }

  int32_t getNext() {
	int32_t ch = source->getNext();
	if (ch < 0)
	  return ch;

	int32_t info = GetUniCharInfo(ch);

	if (info & prohibit) {
	  return -2;
	}
	if (first_ral < 0)
	  first_ral = (info & D1Mask) != 0;

	last_ral = (info & D1Mask) != 0;
	have_ral = have_ral || last_ral;
	have_l = have_l || (info & D2Mask) != 0;

	return ch;
  }

  bool was_valid() {
	return !(have_ral && (!first_ral || !last_ral || have_l));
  }
 private:
  ComposeStream *source;
  int32_t prohibit;
  char first_ral;
  char last_ral;
  char have_ral;
  char have_l;
};

class UTF8Encoder {
 public:
  UTF8Encoder(size_t initial_size, UTF8DecoderStream *input) : input(*input), pos(0) {
	binary.size = initial_size < 4 ? 4 : initial_size;
	binary.data = NULL;
  }

  ~UTF8Encoder() {
	if (binary.data)
	  enif_release_binary(&binary);
  }

  ErlNifBinary *encode_stream(PrepCheckStream *source) {
	int32_t ch, ich;
	int idx = 0;

	while ((ch = source->getNext()) == (ich = input.getNext()) && ch >= 0) {
	  idx++;
	}
	if (ch < -1)
	  return NULL;
	if (ch != ich) {
	  input.reset();
	  while (idx-- > 0)
		if (put_char(input.getNext()) < 0)
		  return NULL;
	  if (ch >= 0) {
		do {
		  if (put_char(ch) < 0)
			return NULL;
		} while ((ch = source->getNext()) >= 0);
		if (ch < -1)
		  return NULL;
	  }
	} else {
	  return input.getBinary();
	}

	if (binary.data) {
	  if (pos != binary.size && !enif_realloc_binary(&binary, pos))
		return NULL;
	} else if (!enif_alloc_binary(0, &binary))
	  return NULL;

	return &binary;
  }

  int put_char(int32_t ch) {
	if (ch <= 0x7F) {
	  if (!buf_size_inc(1)) return -2;
	  binary.data[pos++] = (unsigned char) ch;
	} else if (ch <= 0x7FF) {
	  if (!buf_size_inc(2)) return -2;
	  binary.data[pos] = (unsigned char) ((ch >> 6) | 0xC0);
	  binary.data[pos + 1] = (unsigned char) ((ch | 0x80) & 0xBF);
	  pos += 2;
	} else if (ch <= 0xFFFF) {
	  if (!buf_size_inc(3)) return -2;
	  binary.data[pos] = (unsigned char) ((ch >> 12) | 0xE0);
	  binary.data[pos + 1] = (unsigned char) (((ch >> 6) | 0x80) & 0xBF);
	  binary.data[pos + 2] = (unsigned char) ((ch | 0x80) & 0xBF);
	  pos += 3;
	} else if (ch <= 0x1FFFFF) {
	  if (!buf_size_inc(4)) return -2;
	  binary.data[pos] = (unsigned char) ((ch >> 18) | 0xF0);
	  binary.data[pos + 1] = (unsigned char) (((ch >> 12) | 0x80) & 0xBF);
	  binary.data[pos + 2] = (unsigned char) (((ch >> 6) | 0x80) & 0xBF);
	  binary.data[pos + 3] = (unsigned char) ((ch | 0x80) & 0xBF);
	  pos += 4;
	} else
	  return -2;
	return 0;
  }
 private:
  int buf_size_inc(int inc) {
	int res = 1;

	if (!binary.data)
	  res = enif_alloc_binary(binary.size, &binary);

	if (pos + inc > binary.size)
	  res = enif_realloc_binary(&binary, binary.size * 2);

	return res;
  }

  UTF8DecoderStream input;
  ErlNifBinary binary;
  size_t pos;
};

static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info) {
  return 0;
}

static ERL_NIF_TERM prep(ErlNifEnv *env, int argc,
						 const ERL_NIF_TERM argv[],
						 int prohibit, bool toLower) {
  ErlNifBinary input;

  if (argc != 1)
	return enif_make_badarg(env);

  if (!enif_inspect_iolist_as_binary(env, argv[0], &input))
	return enif_make_badarg(env);

  UTF8DecoderStream decoder(&input);
  PreprocessStream normalize(&decoder, toLower);
  DecompositeStream decomposite(&normalize);
  CanonicalizeStream canonicalize(&decomposite);
  ComposeStream compose(&canonicalize);
  PrepCheckStream prepCheck(&compose, prohibit);
  UTF8Encoder encode(input.size, &decoder);

  ErlNifBinary *res = encode.encode_stream(&prepCheck);

  if (!res || !prepCheck.was_valid()) {
	return enif_make_atom(env, "error");
  } else
	return enif_make_binary(env, res);
}

static ERL_NIF_TERM nodeprep(ErlNifEnv *env, int argc,
							 const ERL_NIF_TERM argv[]) {
  return prep(env, argc, argv, ACMask | C11Mask | C21Mask | XNPMask, 1);
}

static ERL_NIF_TERM nameprep(ErlNifEnv *env, int argc,
							 const ERL_NIF_TERM argv[]) {
  return prep(env, argc, argv, ACMask, 1);
}

static ERL_NIF_TERM resourceprep(ErlNifEnv *env, int argc,
								 const ERL_NIF_TERM argv[]) {
  return prep(env, argc, argv, ACMask | C21Mask, 0);
}

static ERL_NIF_TERM to_lower(ErlNifEnv *env, int argc,
							 const ERL_NIF_TERM argv[]) {
  return prep(env, argc, argv, ACMask, 1);
}

static ErlNifFunc nif_funcs[] =
	{
		{"nodeprep", 1, nodeprep},
		{"nameprep", 1, nameprep},
		{"resourceprep", 1, resourceprep},
		{"tolower", 1, to_lower}
	};

ERL_NIF_INIT(stringprep, nif_funcs, load, NULL, NULL, NULL)
