package modules.admin.Configuration;

import modules.admin.domain.Configuration.PasswordComplexityModel;

/**
 * Manages complexity settings for the system Configuration
 * 
 * @author RB
 */
public class ComplexityModel {
	
	public static int MINIMUM_USERNAME_LENGTH = 4;

	
	private static final String MAXIMUM_COMPARISON = "^(?=.*[A-Z])(?=.*[a-z])(?=.*\\d)(?=.*[^\\p{L}\\p{Nd}]).{8,}$";
	private static String MAXIMUM_PASSWORD_RULE = "Passwords must be 8 characters or more, contain upper and lowercase, numeric characters and punctuation.";

	private static final String MEDIUM_COMPARISON = "^(?=.*\\d)(?=.*[a-z])(?=.*[A-Z]).{6,}$";
	private static String MEDIUM_PASSWORD_RULE = "Passwords must be 6 characters or more and contain upper and lowercase and numeric characters.";

	private static final String MINIMUM_COMPARISON = "^(?=.*[a-zA-Z]).{6,}$";
	private static String MINIMUM_PASSWORD_RULE = "Passwords must be 6 characters or more.";

	public static final PasswordComplexityModel DEFAULT_COMPLEXITY_MODEL = PasswordComplexityModel.minimumMin6Chars;

	private String rule;
	private String comparison;

	public String getRule() {
		return rule;
	}
	
	public String getComparison() {
		return this.comparison;
	}

	public ComplexityModel(PasswordComplexityModel cm){
		switch (cm) {
		case minimumMin6Chars:
			this.rule = MINIMUM_PASSWORD_RULE;
			this.comparison = MINIMUM_COMPARISON;
			break;
		case mediumMin6CharsUpperLowerAndNumeric:
			this.rule = MEDIUM_PASSWORD_RULE;
			this.comparison = MEDIUM_COMPARISON;
			break;
		case maximumMin8CharsUpperLowerNumericAndPunctuation:
			this.rule = MAXIMUM_PASSWORD_RULE;
			this.comparison = MAXIMUM_COMPARISON;
			break;
		default:
		}
	}
}
