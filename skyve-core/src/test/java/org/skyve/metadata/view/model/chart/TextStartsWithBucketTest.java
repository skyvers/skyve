package org.skyve.metadata.view.model.chart;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

class TextStartsWithBucketTest {

	@Test
	@SuppressWarnings("static-method")
	void bizQLExpressionCaseSensitive() {
		TextStartsWithBucket bucket = new TextStartsWithBucket(3, true);
		assertThat(bucket.bizQLExpression("name"), is("substring(bean.name,1,3)"));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizQLExpressionCaseInsensitiveWrapsInUpper() {
		TextStartsWithBucket bucket = new TextStartsWithBucket(1, false);
		assertThat(bucket.bizQLExpression("name"), is("upper(substring(bean.name,1,1))"));
	}

	@Test
	@SuppressWarnings("static-method")
	void labelWithNonNullValue() {
		TextStartsWithBucket bucket = new TextStartsWithBucket(2, true);
		assertThat(bucket.label("AB"), is("AB"));
	}

	@Test
	@SuppressWarnings("static-method")
	void labelWithNullReturnsNull() {
		TextStartsWithBucket bucket = new TextStartsWithBucket(2, true);
		assertNull(bucket.label(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void lengthOf5CaseSensitive() {
		TextStartsWithBucket bucket = new TextStartsWithBucket(5, true);
		assertThat(bucket.bizQLExpression("desc"), is("substring(bean.desc,1,5)"));
	}
}
