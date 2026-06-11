package org.skyve.content;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.Objects;

import org.junit.jupiter.api.Test;

class MimeTypeTest {
	@Test
	@SuppressWarnings("static-method")
	void testMsgFromContentType() {
		// call the method under test
		MimeType result = MimeType.fromContentType("application/vnd.ms-outlook");

		// verify the result
			assertThat(result, is(notNullValue()));
			assertThat(result, is(MimeType.msg));
			assertThat(Objects.requireNonNull(result).getStandardFileSuffix(), is("msg"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMsgFromFileSuffix() {
		// call the method under test
		MimeType result = MimeType.fromFileSuffix("msg");

		// verify the result
			assertThat(result, is(notNullValue()));
			assertThat(result, is(MimeType.msg));
			assertThat(Objects.requireNonNull(result).toString(), is("application/vnd.ms-outlook"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMsgFromFileName() {
		// call the method under test
		MimeType result = MimeType.fromFileName("outlook-message.msg");

		// verify the result
			assertThat(result, is(notNullValue()));
			assertThat(result, is(MimeType.msg));
			MimeType mimeType = Objects.requireNonNull(result);
			assertThat(mimeType.getStandardFileSuffix(), is("msg"));
			assertThat(mimeType.toString(), is("application/vnd.ms-outlook"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMsgFromFileNameUpperCase() {
		// call the method under test
		MimeType result = MimeType.fromFileName("outlook-message.MSG");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result, is(MimeType.msg));
	}
}
