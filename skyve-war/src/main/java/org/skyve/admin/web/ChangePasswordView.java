package org.skyve.admin.web;

import org.skyve.impl.web.faces.views.FacesView;

import com.nulabinc.zxcvbn.Strength;
import com.nulabinc.zxcvbn.Zxcvbn;

import jakarta.faces.view.ViewScoped;
import jakarta.inject.Named;
import modules.admin.domain.ChangePassword;

@ViewScoped
@Named("changePassword")
public class ChangePasswordView extends FacesView {

	private static final long serialVersionUID = 3609442423245595341L;

	private int score = 0;

	public String getPassword() {
		ChangePassword bean = (ChangePassword) getBean();
		if (bean != null) {
			return bean.getNewPassword();
		}
		return null;
	}

	public int getStrengthScore() {
		String password = getPassword();
		if (password != null) {
			Zxcvbn zxcvbn = new Zxcvbn();
			Strength strength = zxcvbn.measure(password);
			score = strength.getScore();
		}
		return score;
	}

	public String getStrengthClass() {
		switch (score) {
			case 0:
            case 1:
				return "progress-bar-danger";
            case 2:
				return "progress-bar-warning";
            case 3:
				return "progress-bar-info";
            case 4:
				return "progress-bar-success";
            default:
				return "progress-bar-default";
        }
	}
}