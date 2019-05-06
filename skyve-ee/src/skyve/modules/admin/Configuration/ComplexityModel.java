package modules.admin.Configuration;

import modules.admin.domain.Configuration.PasswordComplexityModel;

/**
 * Manages complexity settings for the system Configuration
 * 
 * @author RB
 */
public class ComplexityModel {
	
	public static int MINIMUM_USERNAME_LENGTH = 4;

	private static final String STRONG_COMPARISON = "^(?=.*[A-Z])(?=.*[a-z])(?=.*\\d)(?=.*[^\\p{L}\\p{Nd}]).{10,}$";
	private static String STRONG_PASSWORD_RULE = "Passwords must be 10 characters or more, contain upper and lowercase, numeric characters and punctuation.";
	
	private static final String GOOD_COMPARISON = "^(?=.*[A-Z])(?=.*[a-z])(?=.*\\d)(?=.*[^\\p{L}\\p{Nd}]).{8,}$";
	private static String GOOD_PASSWORD_RULE = "Passwords must be 8 characters or more, contain upper and lowercase, numeric characters and punctuation.";

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
		case goodMin8CharsUpperLowerNumericAndPunctuation:
			this.rule = GOOD_PASSWORD_RULE;
			this.comparison = GOOD_COMPARISON;
			break;
		case strongMin10CharsUpperLowerNumericAndPunctuation:
		default:
			this.rule = STRONG_PASSWORD_RULE;
			this.comparison = STRONG_COMPARISON;
			break;
		}
	}
}
