package org.skyve.impl.web.spring;

import java.util.Collection;

import org.skyve.domain.types.Timestamp;
import org.skyve.impl.util.UtilImpl;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Extends the Spring Security user model to include Skyve-specific two-factor authentication and identity metadata.
 */
public class TwoFactorAuthUser extends User {
	private static final long serialVersionUID = 2127717918658548938L;
	
	private String tfaCode;
	private String tfaToken;
	private Timestamp tfaCodeGeneratedTimestamp;
	private String customer;
	private String user;
	private String email;
	private String userPassword;

	/**
	 * Creates a Spring Security user enriched with Skyve two-factor and identity metadata.
	 *
	 * @param username The principal username.
	 * @param password The credential used by Spring Security.
	 * @param enabled Whether the account is enabled.
	 * @param accountNonExpired Whether the account is non-expired.
	 * @param credentialsNonExpired Whether credentials are non-expired.
	 * @param accountNonLocked Whether the account is non-locked.
	 * @param authorities The granted authorities for the principal.
	 * @param customer The effective Skyve customer.
	 * @param user The effective Skyve user name.
	 * @param tfaCode The one-time two-factor code.
	 * @param tfaToken The two-factor challenge token.
	 * @param tfaCodeGeneratedTimestamp The time the current two-factor code was generated.
	 * @param email The principal email address.
	 * @param userPassword The original user password value for downstream flows.
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public TwoFactorAuthUser(@Nonnull String username,
					@Nonnull String password,
					boolean enabled,
					boolean accountNonExpired,
					boolean credentialsNonExpired,
					boolean accountNonLocked,
					@Nonnull Collection<? extends GrantedAuthority> authorities,
					@Nullable String customer,
					@Nonnull String user,
					@Nullable String tfaCode,
					@Nullable String tfaToken,
					@Nullable Timestamp tfaCodeGeneratedTimestamp,
					@Nullable String email,
					@Nullable String userPassword) {
		super(username, password, enabled, accountNonExpired, credentialsNonExpired, accountNonLocked, authorities);
		this.tfaCode = UtilImpl.processStringValue(tfaCode);
		this.tfaToken = UtilImpl.processStringValue(tfaToken);
		this.tfaCodeGeneratedTimestamp = tfaCodeGeneratedTimestamp;
		this.customer = UtilImpl.processStringValue(customer);
		this.user = UtilImpl.processStringValue(user);
		this.email = UtilImpl.processStringValue(email);
		this.userPassword = userPassword;
	}

	/**
	 * Returns the generated one-time two-factor code.
	 *
	 * @return The current two-factor code.
	 */
	public @Nullable String getTfaCode() {
		return tfaCode;
	}

	/**
	 * Updates the one-time two-factor code.
	 *
	 * @param tfaCode The new two-factor code.
	 */
	public void setTfaCode(@Nullable String tfaCode) {
		this.tfaCode = UtilImpl.processStringValue(tfaCode);
	}

	/**
	 * Returns the server-issued two-factor challenge token.
	 *
	 * @return The current two-factor challenge token.
	 */
	public @Nullable String getTfaToken() {
		return tfaToken;
	}

	/**
	 * Updates the server-issued two-factor challenge token.
	 *
	 * @param tfaToken The new two-factor challenge token.
	 */
	public void setTfaToken(@Nullable String tfaToken) {
		this.tfaToken = UtilImpl.processStringValue(tfaToken);
	}

	/**
	 * Returns the timestamp when the current two-factor code was generated.
	 *
	 * @return The timestamp for the current two-factor code generation.
	 */
	public @Nullable Timestamp getTfaCodeGeneratedTimestamp() {
		return tfaCodeGeneratedTimestamp;
	}

	/**
	 * Sets the timestamp when the current two-factor code was generated.
	 *
	 * @param tfaCodeGeneratedTimestamp The timestamp for the current two-factor code generation.
	 */
	public void setTfaCodeGeneratedTimestamp(@Nullable Timestamp tfaCodeGeneratedTimestamp) {
		this.tfaCodeGeneratedTimestamp = tfaCodeGeneratedTimestamp;
	}

	/**
	 * Returns the effective customer for this authenticated principal.
	 *
	 * @return The effective customer.
	 */
	public @Nullable String getCustomer() {
		return customer;
	}

	/**
	 * Returns the Skyve user name for this authenticated principal.
	 *
	 * @return The Skyve user name.
	 */
	public @Nullable String getUser() {
		return user;
	}

	/**
	 * Returns the email address associated with this authenticated principal.
	 *
	 * @return The principal email address.
	 */
	public @Nullable String getEmail() {
		return email;
	}

	/**
	 * Returns the original user password value carried for downstream authentication flows.
	 *
	 * @return The original user password.
	 */
	public @Nullable String getUserPassword() {
		return userPassword;
	}
}
