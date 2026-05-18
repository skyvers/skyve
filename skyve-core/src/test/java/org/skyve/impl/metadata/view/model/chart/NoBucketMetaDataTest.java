package org.skyve.impl.metadata.view.model.chart;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

class NoBucketMetaDataTest {

	@Test
	@SuppressWarnings("static-method")
	void bizQLExpressionReturnsBeanDotBinding() {
		NoBucketMetaData bucket = new NoBucketMetaData();
		assertThat(bucket.bizQLExpression("name"), is("bean.name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizQLExpressionNestedBinding() {
		NoBucketMetaData bucket = new NoBucketMetaData();
		assertThat(bucket.bizQLExpression("contact.name"), is("bean.contact.name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void labelReturnsToStringOfCategory() {
		NoBucketMetaData bucket = new NoBucketMetaData();
		assertThat(bucket.label("hello"), is("hello"));
	}

	@Test
	@SuppressWarnings("static-method")
	void labelReturnsNullForNull() {
		NoBucketMetaData bucket = new NoBucketMetaData();
		assertNull(bucket.label(null));
	}
}
