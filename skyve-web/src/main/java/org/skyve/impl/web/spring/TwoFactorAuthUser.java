package org.skyve.impl.web.spring;

import java.util.Collection;

import org.skyve.domain.types.Timestamp;
import org.skyve.impl.util.UtilImpl;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.User;

/**
 * Spring Security user details carrying Skyve-specific MFA and identity fields
 * required during two-factor authentication flows.
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
	private String mfaConfiguration;

	/**
	 * Create a user details instance with two-factor authentication state.
	 *
	 * @param username the Spring Security username (customer/user)
	 * @param password the password used by Spring authentication for this request
	 * @param enabled whether the account is enabled
	 * @param accountNonExpired whether the account is non-expired
	 * @param credentialsNonExpired whether credentials are non-expired
	 * @param accountNonLocked whether the account is non-locked
	 * @param authorities granted authorities
	 * @param customer the Skyve customer name
	 * @param user the Skyve user name
	 * @param tfaCode the stored hashed MFA code
	 * @param tfaToken the MFA token for the active challenge
	 * @param tfaCodeGeneratedTimestamp when the MFA code was generated
	 * @param email the user email address used for push MFA
	 * @param userPassword the user's persisted password hash
	 * @param mfaConfiguration per-user MFA JSON configuration
	 */
	public TwoFactorAuthUser(String username,
					String password,
					boolean enabled,
					boolean accountNonExpired,
					boolean credentialsNonExpired,
					boolean accountNonLocked,
					Collection<? extends GrantedAuthority> authorities,
					String customer,
					String user,
					String tfaCode,
					String tfaToken,
					Timestamp tfaCodeGeneratedTimestamp,
					String email,
					String userPassword,
					String mfaConfiguration) {
		super(username, password, enabled, accountNonExpired, credentialsNonExpired, accountNonLocked, authorities);
		this.tfaCode = UtilImpl.processStringValue(tfaCode);
		this.tfaToken = UtilImpl.processStringValue(tfaToken);
		this.tfaCodeGeneratedTimestamp = tfaCodeGeneratedTimestamp;
		this.customer = UtilImpl.processStringValue(customer);
		this.user = UtilImpl.processStringValue(user);
		this.email = UtilImpl.processStringValue(email);
		this.userPassword = userPassword;
		this.mfaConfiguration = UtilImpl.processStringValue(mfaConfiguration);
	}

	/**
	 * @return the stored hashed MFA code
	 */
	public String getTfaCode() {
		return tfaCode;
	}

	/**
	 * @param tfaCode the stored hashed MFA code
	 */
	public void setTfaCode(String tfaCode) {
		this.tfaCode = UtilImpl.processStringValue(tfaCode);
	}

	/**
	 * @return the MFA token for the active challenge
	 */
	public String getTfaToken() {
		return tfaToken;
	}

	/**
	 * @param tfaToken the MFA token for the active challenge
	 */
	public void setTfaToken(String tfaToken) {
		this.tfaToken = UtilImpl.processStringValue(tfaToken);
	}

	/**
	 * @return when the MFA code was generated
	 */
	public Timestamp getTfaCodeGeneratedTimestamp() {
		return tfaCodeGeneratedTimestamp;
	}

	/**
	 * @param tfaCodeGeneratedTimestamp when the MFA code was generated
	 */
	public void setTfaCodeGeneratedTimestamp(Timestamp tfaCodeGeneratedTimestamp) {
		this.tfaCodeGeneratedTimestamp = tfaCodeGeneratedTimestamp;
	}

	/**
	 * @return the Skyve customer name
	 */
	public String getCustomer() {
		return customer;
	}

	/**
	 * @return the Skyve user name
	 */
	public String getUser() {
		return user;
	}

	/**
	 * @return the user email address
	 */
	public String getEmail() {
		return email;
	}

	/**
	 * @return the user's persisted password hash
	 */
	public String getUserPassword() {
		return userPassword;
	}

	/**
	 * @return per-user MFA JSON configuration
	 */
	public String getMfaConfiguration() {
		return mfaConfiguration;
	}
}
