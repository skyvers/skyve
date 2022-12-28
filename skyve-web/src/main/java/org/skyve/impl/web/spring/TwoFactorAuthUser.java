package org.skyve.impl.web.spring;

import java.util.Collection;

import org.skyve.domain.types.Timestamp;
import org.skyve.impl.util.UtilImpl;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.User;

public class TwoFactorAuthUser extends User {
	private static final long serialVersionUID = 2127717918658548938L;
	
	private String tfaCode;
	private String tfaToken;
	private Timestamp tfaCodeGeneratedTimestamp;
	private String customer;
	private String user;
	private String email;
	private String userPassword;

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
					String userPassword) {
		super(username, password, enabled, accountNonExpired, credentialsNonExpired, accountNonLocked, authorities);
		this.tfaCode = UtilImpl.processStringValue(tfaCode);
		this.tfaToken = UtilImpl.processStringValue(tfaToken);
		this.tfaCodeGeneratedTimestamp = tfaCodeGeneratedTimestamp;
		this.customer = UtilImpl.processStringValue(customer);
		this.user = UtilImpl.processStringValue(user);
		this.email = UtilImpl.processStringValue(email);
		this.userPassword = userPassword;
	}

	public String getTfaCode() {
		return tfaCode;
	}

	public void setTfaCode(String tfaCode) {
		this.tfaCode = UtilImpl.processStringValue(tfaCode);
	}

	public String getTfaToken() {
		return tfaToken;
	}

	public void setTfaToken(String tfaToken) {
		this.tfaToken = UtilImpl.processStringValue(tfaToken);
	}

	public Timestamp getTfaCodeGeneratedTimestamp() {
		return tfaCodeGeneratedTimestamp;
	}

	public void setTfaCodeGeneratedTimestamp(Timestamp tfaCodeGeneratedTimestamp) {
		this.tfaCodeGeneratedTimestamp = tfaCodeGeneratedTimestamp;
	}

	public String getCustomer() {
		return customer;
	}

	public String getUser() {
		return user;
	}

	public String getEmail() {
		return email;
	}

	public String getUserPassword() {
		return userPassword;
	}
}
