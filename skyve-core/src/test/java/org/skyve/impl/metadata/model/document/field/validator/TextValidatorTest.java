package org.skyve.impl.metadata.model.document.field.validator;

import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator.ValidatorType;
import org.skyve.impl.metadata.user.SuperUser;

public class TextValidatorTest {

	private TextValidator validator;
	private SuperUser user;

	@BeforeEach
	public void before() {
		validator = new TextValidator();
		user = new SuperUser();
	}

	@Test
	public void testValidateCreditCardInvalid() throws Exception {
		// setup the test data
		validator.setType(ValidatorType.creditCard);

		String[] testCardNumbers = new String[] {
				"123456789012345", // 15 digis
				"#@%^%#$@#$@#", // Garbage
		};

		// call the method under test
		for (String cardNumber : testCardNumbers) {
			ValidationException e = new ValidationException();
			validator.validate(user, cardNumber, "binding", "Binding", null, e);

			// verify the result
			if (e.getMessages().isEmpty()) {
				fail("Card number " + cardNumber + " should not have passed validation.");
			}
		}
	}

	@Test
	public void testValidateCreditCardValid() throws Exception {
		// setup the test data
		validator.setType(ValidatorType.creditCard);

		String[] testCardNumbers = new String[] {
				"4111111111111111", // visa
				"5431111111111111", // mastercard
				"378734493671000", // amex
				"6011111111111117", // discover
		};

		// call the method under test
		for (String cardNumber : testCardNumbers) {
			ValidationException e = new ValidationException();
			validator.validate(user, cardNumber, "binding", "Binding", null, e);

			// verify the result
			if (!e.getMessages().isEmpty()) {
				fail("Card number " + cardNumber + " should have passed validation.");
			}
		}
	}

	@Test
	public void testValidateEmailInvalid() throws Exception {
		// setup the test data
		validator.setType(ValidatorType.email);

		String[] testEmails = new String[] {
				"plainaddress", // Missing @ sign and domain
				"#@%^%#$@#$@#.com", // Garbage
				"@domain.com", // Missing username
				"email.domain.com", // Missing @
				"email@domain@domain.com", // Two @ sign
		};

		// call the method under test
		for (String email : testEmails) {
			ValidationException e = new ValidationException();
			validator.validate(user, email, "binding", "Binding", null, e);

			// verify the result
			if (e.getMessages().isEmpty()) {
				fail("Email " + email + " should not have passed validation.");
			}
		}
	}

	@Test
	public void testValidateEmailValid() throws Exception {
		// setup the test data
		validator.setType(ValidatorType.email);

		String[] testEmails = new String[] {
				"email@domain.com", // Valid email
				"firstname.lastname@domain.com", // Email contains dot in the address field
				"email@subdomain.domain.com", // Email contains dot with subdomain
				"firstname+lastname@domain.com", // Plus sign is considered valid character
				"email@123.123.123.123", // Domain is valid IP address
				"email@[123.123.123.123]", // Square bracket around IP address is considered valid
				"\"email\"@domain.com", // Quotes around email is considered valid
				"1234567890@domain.com", // Digits in address are valid
				"email@domain-one.com", // Dash in domain name is valid
				"_______@domain.com", // Underscore in the address field is valid
				"email@domain.name", // .name is valid Top Level Domain name
				"email@domain.co.jp", // Dot in Top Level Domain name also considered valid (use co.jp as example here)
				"firstname-lastname@domain.com", // Dash in address field is valid
				"email@domain", // Missing top level domain (.com/.net/.org/etc)
				"email@domain.fabricatedfortest", // custom tld
		};

		// call the method under test
		for (String email : testEmails) {
			ValidationException e = new ValidationException();
			validator.validate(user, email, "binding", "Binding", null, e);

			if (!e.getMessages().isEmpty()) {
				fail("Email " + email + " should have passed validation.");
			}
		}
	}

	@Test
	public void testValidateUrlInvalid() {
		// setup the test data
		validator.setType(ValidatorType.url);

		String[] testUrls = new String[] {
				"http://skyve",
				"https://skyve",
				"www.skyve",
				"skyve"
		};

		// call the method under test
		for (String url : testUrls) {
			ValidationException e = new ValidationException();
			validator.validate(user, url, "binding", "Binding", null, e);

			// verify the result
			if (e.getMessages().isEmpty()) {
				fail("Url " + url + " should not have passed validation.");
			}
		}
	}

	@Test
	public void testValidateUrlValid() {
		// setup the test data
		validator.setType(ValidatorType.url);

		String[] testUrls = new String[] {
				"http://skyve.org",
				"https://skyve.org",
				"ftp://skyve.org",
				"http://foundry.skyve.org",
				"https://foundry.skyve.org",
				"http://www.skyve.org",
				"https://www.skyve.org",
				"www.skyve.org",
				"skyve.org"
		};

		// call the method under test
		for (String url : testUrls) {
			ValidationException e = new ValidationException();
			validator.validate(user, url, "binding", "Binding", null, e);

			// verify the result
			if (!e.getMessages().isEmpty()) {
				fail("Url " + url + " should have passed validation.");
			}
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSetTypeInternetDomainSetsRegularExpression() {
		TextValidator tv = new TextValidator();
		tv.setType(ValidatorType.internetDomain);
		// internetDomain does not auto-set regularExpression — type is stored
		assertEquals(ValidatorType.internetDomain, tv.getType());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testValidateInternetDomainValid() {
		TextValidator tv = new TextValidator();
		tv.setType(ValidatorType.internetDomain);
		ValidationException e = new ValidationException();
		tv.validate(new SuperUser(), "skyve.org", "binding", "Domain", null, e);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testValidateInternetDomainInvalid() {
		TextValidator tv = new TextValidator();
		tv.setType(ValidatorType.internetDomain);
		ValidationException e = new ValidationException();
		tv.validate(new SuperUser(), "not-a-valid-domain", "binding", "Domain", null, e);
		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testValidateIpAddressValid() {
		TextValidator tv = new TextValidator();
		tv.setType(ValidatorType.ipAddress);
		ValidationException e = new ValidationException();
		tv.validate(new SuperUser(), "192.168.1.1", "binding", "IP", null, e);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testValidateIpv4AddressValid() {
		TextValidator tv = new TextValidator();
		tv.setType(ValidatorType.ipv4Address);
		ValidationException e = new ValidationException();
		tv.validate(new SuperUser(), "10.0.0.1", "binding", "IP", null, e);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testValidateEan13CheckDigitValid() {
		TextValidator tv = new TextValidator();
		tv.setType(ValidatorType.ean13CheckDigit);
		ValidationException e = new ValidationException();
		// EAN13 barcode for "978020137962" + check digit 4
		tv.validate(new SuperUser(), "9780201379624", "binding", "EAN13", null, e);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testValidateIbanCheckDigitValid() {
		TextValidator tv = new TextValidator();
		tv.setType(ValidatorType.ibanCheckDigit);
		ValidationException e = new ValidationException();
		// Valid GB IBAN
		tv.validate(new SuperUser(), "GB82WEST12345698765432", "binding", "IBAN", null, e);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testValidateLuhnCheckDigitValid() {
		TextValidator tv = new TextValidator();
		tv.setType(ValidatorType.luhnCheckDigit);
		ValidationException e = new ValidationException();
		// Standard Luhn valid number (Visa test card)
		tv.validate(new SuperUser(), "4111111111111111", "binding", "Luhn", null, e);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testValidateNullValueDoesNothing() {
		TextValidator tv = new TextValidator();
		tv.setType(ValidatorType.email);
		ValidationException e = new ValidationException();
		tv.validate(new SuperUser(), null, "binding", "Email", null, e);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testValidateRegularExpressionMatch() {
		TextValidator tv = new TextValidator();
		tv.setRegularExpression("^[A-Z]{3}$");
		ValidationException e = new ValidationException();
		tv.validate(new SuperUser(), "ABC", "binding", "Code", null, e);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testValidateRegularExpressionNoMatch() {
		TextValidator tv = new TextValidator();
		tv.setRegularExpression("^[A-Z]{3}$");
		ValidationException e = new ValidationException();
		tv.validate(new SuperUser(), "abc", "binding", "Code", null, e);
		assertFalse(e.getMessages().isEmpty());
	}

}

