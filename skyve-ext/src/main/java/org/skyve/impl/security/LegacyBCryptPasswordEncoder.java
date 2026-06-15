package org.skyve.impl.security;

import java.nio.charset.StandardCharsets;

import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;

/**
 * Preserves bcrypt compatibility with historical behavior where inputs longer than
 * 72 UTF-8 bytes are truncated before hashing and comparison.
 */
public class LegacyBCryptPasswordEncoder implements PasswordEncoder {
	private static final int BCRYPT_MAX_PASSWORD_BYTES = 72;

	private final BCryptPasswordEncoder delegate = new BCryptPasswordEncoder();

	@Override
	public String encode(CharSequence rawPassword) {
		return delegate.encode(normalizeRawPassword(rawPassword));
	}

	@Override
	public boolean matches(CharSequence rawPassword, String encodedPassword) {
		return delegate.matches(normalizeRawPassword(rawPassword), encodedPassword);
	}

	@Override
	public boolean upgradeEncoding(String encodedPassword) {
		return delegate.upgradeEncoding(encodedPassword);
	}

	private static String normalizeRawPassword(CharSequence rawPassword) {
		if (rawPassword == null) {
			throw new IllegalArgumentException("rawPassword cannot be null");
		}

		String value = rawPassword.toString();
		if (value.getBytes(StandardCharsets.UTF_8).length <= BCRYPT_MAX_PASSWORD_BYTES) {
			return value;
		}

		int byteCount = 0;
		StringBuilder truncated = new StringBuilder(value.length());
		for (int offset = 0; offset < value.length();) {
			int codePoint = value.codePointAt(offset);
			int bytesForCodePoint = utf8ByteLength(codePoint);
			if ((byteCount + bytesForCodePoint) > BCRYPT_MAX_PASSWORD_BYTES) {
				break;
			}

			truncated.appendCodePoint(codePoint);
			byteCount += bytesForCodePoint;
			offset += Character.charCount(codePoint);
		}

		return truncated.toString();
	}

	private static int utf8ByteLength(int codePoint) {
		if (codePoint <= 0x7F) {
			return 1;
		}
		if (codePoint <= 0x7FF) {
			return 2;
		}
		if (codePoint <= 0xFFFF) {
			return 3;
		}
		return 4;
	}
}
