package org.skyve.impl.security;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.apache.commons.codec.binary.Base64;
import org.skyve.util.Util;
import org.springframework.security.crypto.password.PasswordEncoder;

/**
 * Enable unsalted MD5 and SHA1 password encoding and matching for back compat.
 * @author mike
 */
public class SkyveLegacyPasswordEncoder implements PasswordEncoder {
	@Override
	public String encode(CharSequence rawPassword) {
		return encode(rawPassword, Util.getPasswordHashingAlgorithm());
	}
	
	@Override
	public boolean matches(CharSequence rawPassword, String encodedPassword) {
		int encodedPasswordLength = encodedPassword.length();
		String passwordHashingAlgorithm = Util.getPasswordHashingAlgorithm();
		if (encodedPasswordLength == 28) {
			passwordHashingAlgorithm = "SHA1";
		}
		else if (encodedPasswordLength == 24) {
			passwordHashingAlgorithm = "MD5";
		}
		return matches(rawPassword, encodedPassword, passwordHashingAlgorithm);
	}
	
	public static String encode(CharSequence rawPassword, String passwordHashingAlgorithm) {
		try {
			MessageDigest md = MessageDigest.getInstance(passwordHashingAlgorithm);
			Base64 base64Codec = new Base64();
			return new String(base64Codec.encode(md.digest(rawPassword.toString().getBytes())));
		}
		catch (@SuppressWarnings("unused") NoSuchAlgorithmException e) {
			return null;
		}
	}
	
	public static boolean matches(CharSequence rawPassword, String encodedPassword, String passwordHashingAlgorithm) {
		return encodedPassword.equals(encode(rawPassword, passwordHashingAlgorithm));
	}
}
