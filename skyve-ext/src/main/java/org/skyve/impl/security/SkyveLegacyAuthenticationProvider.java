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
import org.skyve.util.Util;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.InternalAuthenticationServiceException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;

/**
 * Authenticates users against legacy hashed-password storage using direct SQL lookup.
 */
public class SkyveLegacyAuthenticationProvider implements AuthenticationProvider {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(SkyveLegacyAuthenticationProvider.class);

	/**
	 * SQL statement used to retrieve a user's stored hashed password by username.
	 */
	private String hashedPasswordSql;
	
	/**
	 * Authenticates supplied credentials by hashing the presented password and comparing against
	 * the configured user-password query result.
	 *
	 * @param authentication The authentication request.
	 * @return An authenticated token when credentials are valid.
	 * @throws AuthenticationException If the credentials are invalid or authentication cannot be completed.
	 */
	@Override
	public Authentication authenticate(Authentication authentication) throws AuthenticationException {
		String name = authentication.getName();
		String password = authentication.getCredentials().toString();

		MessageDigest md;
		try {
			md = MessageDigest.getInstance(Util.getPasswordHashingAlgorithm());
		}
		catch (NoSuchAlgorithmException e) {
			LOGGER.error("Could not authenticate user {}. No such hashing algorithm {}", name, Util.getPasswordHashingAlgorithm());
			throw new InternalAuthenticationServiceException("Could not authenticate user " + name, e);
		}
		Base64 base64Codec = new Base64();
		String hashedPassword = new String(base64Codec.encode(md.digest(password.getBytes())));

		/*
		 * If we were to convert passwords automatically
		 * 1) retrieve the hashed password
		 * 2) if password length is 24 then check stored MD5 hashed password
		 * 3) else if password length is 28 then check stored SHA1 hashed password
		 * 4) if the password hashes match, take the clear-text password above, rehash and store
		 */
		try (Connection c = EXT.getDataStoreConnection()) {
			try (PreparedStatement s = c.prepareStatement(hashedPasswordSql)) {
				s.setString(1, name);
				try (ResultSet rs = s.executeQuery()) {
					if (rs.next()) {
						String storedPasswordHash = rs.getString(1);
						if (storedPasswordHash == null) {
							LOGGER.error("User {} has no password", name);
							throw new InternalAuthenticationServiceException("User " + name + " has no password");
						}
						if (rs.next()) {
							LOGGER.error("User {} is not unique", name);
							throw new InternalAuthenticationServiceException("User " + name + " is not unique");
						}
						if (! storedPasswordHash.equals(hashedPassword)) {
							LOGGER.error("Password mismatch for user {}", name);
							throw new BadCredentialsException("Invalid Credential");
						}
					}
					else {
						LOGGER.error("User {} not found", name);
						throw new BadCredentialsException("Invalid Credential");
					}
				}
			}
		}
		catch (SQLException e) {
			throw new InternalAuthenticationServiceException("Could not authenticate user " + name, e);
		}

		UsernamePasswordAuthenticationToken result = new UsernamePasswordAuthenticationToken(name, authentication.getCredentials(), Collections.emptyList());
		result.setDetails(authentication.getDetails());
		return result;
	}

	/**
	 * Indicates whether this provider can process the supplied authentication token type.
	 *
	 * @param authentication The authentication token class.
	 * @return {@code true}, as this provider accepts all authentication types.
	 */
	@Override
	public boolean supports(Class<?> authentication) {
		// NB - will implement all authentication mechanisms over time but for now
		// org.springframework.security.authentication.UsernamePasswordAuthenticationToken is supported
		return true;
	}
	
	/**
	 * Returns the configured SQL query used to look up stored password hashes.
	 *
	 * @return The configured password-hash lookup SQL.
	 */
	public String getHashedPasswordSql() {
		return hashedPasswordSql;
	}
	
	/**
	 * Sets the SQL query used to retrieve a stored password hash for a username.
	 *
	 * @param hashedPasswordSql The password-hash lookup SQL.
	 */
	public void setHashedPasswordSql(String hashedPasswordSql) {
		this.hashedPasswordSql = hashedPasswordSql;
	}
}
