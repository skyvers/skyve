package org.skyve.impl.bind;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute;

public class BindUtilTest {

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaInstanceIdentifierLeavesValidIndentifier() {
		// setup the test data
		String identifier = "validIdentifier";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is(identifier));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaInstanceIdentifierRemovesInvalidCharacters() {
		// setup the test data
		String identifier = "E-Mail";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is("EMail"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaInstanceIdentifierRemovesWhitespace() {
		// setup the test data
		String identifier = "Whitespace Identifier";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is("whitespaceIdentifier"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaInstanceIdentifierPreservesTwoLeadingCapitals() {
		// setup the test data
		String identifier = "DOB";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is("DOB"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaTypeIdentifierRemovesInvalidCharacters() {
		// setup the test data
		String identifier = "E-Mail";

		// call the method under test
		String result = BindUtil.toJavaTypeIdentifier(identifier);

		// verify the result
		assertThat(result, is("EMail"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaTypeIdentifierRemovesWhitespace() {
		// setup the test data
		String identifier = "Whitespace Identifier";

		// call the method under test
		String result = BindUtil.toJavaTypeIdentifier(identifier);

		// verify the result
		assertThat(result, is("WhitespaceIdentifier"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaTypeIdentifierPreservesTwoLeadingCapitals() {
		// setup the test data
		String identifier = "DOB";

		// call the method under test
		String result = BindUtil.toJavaTypeIdentifier(identifier);

		// verify the result
		assertThat(result, is("DOB"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaStaticIdentifierRemovesWhitespace() {
		// setup the test data
		String identifier = "Whitespace Identifier";

		// call the method under test
		String result = BindUtil.toJavaStaticIdentifier(identifier);

		// verify the result
		assertThat(result, is("WHITESPACE_IDENTIFIER"));
	}

	/**
	 * Two leading capitals shouldn't have any effect on the static identifier, this
	 * just makes sure the Introspector.decapitalize has no negative effects.
	 */
	@Test
	@SuppressWarnings("static-method")
	public void testToJavaStaticIdentifierPreservesTwoLeadingCapitals() {
		// setup the test data
		String identifier = "DOB";

		// call the method under test
		String result = BindUtil.toJavaStaticIdentifier(identifier);

		// verify the result
		assertThat(result, is("DOB"));
	}

	@Test
	public void testGetImplementingTypeForGenerateDomainValidationReturnsResolvedType() {
		Attribute attribute = mock(Attribute.class);
		doReturn(String.class).when(attribute).getImplementingType();

		assertEquals(String.class, BindUtil.getImplementingTypeForGenerateDomainValidation(attribute));
	}

	@Test
	public void testGetImplementingTypeForGenerateDomainValidationFallsBackToEnumClass() {
		Enumeration enumeration = mock(Enumeration.class);
		doThrow(new MetaDataException("Enum class is not generated yet")).when(enumeration).getImplementingType();

		assertEquals(Enum.class, BindUtil.getImplementingTypeForGenerateDomainValidation(enumeration));
	}

	@Test
	public void testGetImplementingTypeForGenerateDomainValidationStillThrowsForNonEnums() {
		Attribute attribute = mock(Attribute.class);
		doThrow(new MetaDataException("Non-enum metadata error")).when(attribute).getImplementingType();

		assertThrows(MetaDataException.class,
				() -> BindUtil.getImplementingTypeForGenerateDomainValidation(attribute));
	}
}
