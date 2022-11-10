package org.skyve.impl.web.spring;

import java.util.Collection;

import org.skyve.domain.types.DateTime;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.User;


public class UserTFA extends User {
	
	
	private static final long serialVersionUID = 2127717918658548938L;
	
	private String tfaCode;
	private String tfaToken;
	private DateTime tfaCodeGeneratedDateTime;
	private String customer;
	// "super.username" customer/user 
	// skye customer needs to be passed in with username so we know what to query 
	private String user;
	private String email;

	public UserTFA(String username, String password, boolean enabled, boolean accountNonExpired,
			boolean credentialsNonExpired, boolean accountNonLocked,
			Collection<? extends GrantedAuthority> authorities,
			String customer,  String user, String tfaCode, String tfaToken,  DateTime tfaCodeGeneratedDateTime, String email ) {
		super(username, password, enabled, accountNonExpired, credentialsNonExpired, accountNonLocked, authorities);
		this.tfaCode = tfaCode;
		this.tfaToken = tfaToken;
		this.tfaCodeGeneratedDateTime = tfaCodeGeneratedDateTime;
		this.customer = customer;
		this.user = user;
		this.email = email;
	}


	public String getTfaCode() {
		return tfaCode;
	}

	public String getTfaToken() {
		return tfaToken;
	}

	public DateTime getTfaCodeGeneratedDateTime() {
		return tfaCodeGeneratedDateTime;
	}

	public void setTfaCode(String tfaCode) {
		this.tfaCode = tfaCode;
	}

	public void setTfaToken(String tfaToken) {
		this.tfaToken = tfaToken;
	}

	public void setTfaCodeGeneratedDateTime(DateTime tfaCodeGeneratedDateTime) {
		this.tfaCodeGeneratedDateTime = tfaCodeGeneratedDateTime;
	}

	public String getCustomer() {
		return customer;
	}

	public void setCustomer(String customer) {
		this.customer = customer;
	}

	public String getUser() {
		return user;
	}

	public void setUser(String user) {
		this.user = user;
	}
	public String getEmail() {
		return email;
	}
	public void setEmail(String email) {
		this.email = email;
	}
}
