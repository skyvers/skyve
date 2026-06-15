package org.skyve.metadata.view.model.chart;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

class TextLengthBucketTest {

	@Test
	@SuppressWarnings("static-method")
	void bizQLExpressionWrapsBinding() {
		TextLengthBucket bucket = new TextLengthBucket();
		assertThat(bucket.bizQLExpression("name"), is("length(bean.name)"));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizQLExpressionNestedBinding() {
		TextLengthBucket bucket = new TextLengthBucket();
		assertThat(bucket.bizQLExpression("contact.name"), is("length(bean.contact.name)"));
	}

	@Test
	@SuppressWarnings("static-method")
	void labelReturnsToStringOfCategory() {
		TextLengthBucket bucket = new TextLengthBucket();
		assertThat(bucket.label(Integer.valueOf(5)), is("5"));
	}

	@Test
	@SuppressWarnings("static-method")
	void labelReturnsNullForNullCategory() {
		TextLengthBucket bucket = new TextLengthBucket();
		assertNull(bucket.label(null));
	}
}
