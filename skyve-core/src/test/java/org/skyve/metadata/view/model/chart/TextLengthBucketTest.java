package org.skyve.metadata.view.model.chart;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

public class TextLengthBucketTest {

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionWrapsBinding() {
		TextLengthBucket bucket = new TextLengthBucket();
		assertThat(bucket.bizQLExpression("name"), is("length(bean.name)"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionNestedBinding() {
		TextLengthBucket bucket = new TextLengthBucket();
		assertThat(bucket.bizQLExpression("contact.name"), is("length(bean.contact.name)"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelReturnsToStringOfCategory() {
		TextLengthBucket bucket = new TextLengthBucket();
		assertThat(bucket.label(Integer.valueOf(5)), is("5"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelReturnsNullForNullCategory() {
		TextLengthBucket bucket = new TextLengthBucket();
		assertNull(bucket.label(null));
	}
}
