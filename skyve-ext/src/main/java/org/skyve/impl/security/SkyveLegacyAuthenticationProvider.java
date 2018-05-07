package org.skyve.impl.security;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collections;

import org.apache.commons.codec.binary.Base64;
import org.skyve.EXT;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.InternalAuthenticationServiceException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;

public class SkyveLegacyAuthenticationProvider implements AuthenticationProvider {
	private String hashedPasswordSql;
	
	@Override
	public Authentication authenticate(Authentication authentication) throws AuthenticationException {
		String name = authentication.getName();
		String password = authentication.getCredentials().toString();

		MessageDigest md;
		try {
			md = MessageDigest.getInstance(Util.getPasswordHashingAlgorithm());
		}
		catch (NoSuchAlgorithmException e) {
			UtilImpl.LOGGER.severe("Could not authenticate user " + name + ". No such hashing algorithm " + Util.getPasswordHashingAlgorithm());
			throw new InternalAuthenticationServiceException("Could not authenticate user " + name, e);
		}
		Base64 base64Codec = new Base64();
		String hashedPassword = new String(base64Codec.encode(md.digest(password.getBytes())));

		/*
		 * If we were to convert passwords automatically
		 * 1) retrieve the hashed password
		 * 2) if (password length == 24) then check stored MD5 hashed password
		 * 3) else if (password length == 28) then check stored SHA1 hashed password
		 * 4) if the password hashes match, take the clear-text password above, rehash and store
		 */
		try (Connection c = EXT.getDataStoreConnection()) {
			try (PreparedStatement s = c.prepareStatement(hashedPasswordSql)) {
				s.setString(1, name);
				try (ResultSet rs = s.executeQuery()) {
					if (rs.next()) {
						String storedPasswordHash = rs.getString(1);
						if (storedPasswordHash == null) {
							UtilImpl.LOGGER.severe("User " + name + " has no password");
							throw new InternalAuthenticationServiceException("User " + name + " has no password");
						}
						if (rs.next()) {
							UtilImpl.LOGGER.severe("User " + name + " is not unique");
							throw new InternalAuthenticationServiceException("User " + name + " is not unique");
						}
						if (! storedPasswordHash.equals(hashedPassword)) {
							UtilImpl.LOGGER.severe("Password mismatch for user " + name);
							throw new BadCredentialsException("Invalid Credential");
						}
					}
					else {
						UtilImpl.LOGGER.severe("User " + name + " not found");
						throw new BadCredentialsException("Invalid Credential");
					}
				}
			}
		}
		catch (SQLException e) {
			throw new InternalAuthenticationServiceException("Could not authenticate user " + name, e);
		}

		UsernamePasswordAuthenticationToken result = new UsernamePasswordAuthenticationToken(name, authentication.getCredentials(), Collections.EMPTY_LIST);
		result.setDetails(authentication.getDetails());
		return result;
	}

	@Override
	public boolean supports(Class<?> authentication) {
		// NB - will implement all authentication mechanisms over time but for now
		// org.springframework.security.authentication.UsernamePasswordAuthenticationToken is supported
		return true;
	}
	
	public String getHashedPasswordSql() {
		return hashedPasswordSql;
	}
	
	public void setHashedPasswordSql(String hashedPasswordSql) {
		this.hashedPasswordSql = hashedPasswordSql;
	}
}
