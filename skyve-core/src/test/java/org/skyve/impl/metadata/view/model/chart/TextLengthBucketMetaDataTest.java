package org.skyve.impl.metadata.view.model.chart;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class TextLengthBucketMetaDataTest {

	@Test
	void constructorCreatesInstance() {
		TextLengthBucketMetaData md = new TextLengthBucketMetaData();
		assertNotNull(md);
	}

	@Test
	void bizQLExpressionDelegatesToSuperClass() {
		TextLengthBucketMetaData md = new TextLengthBucketMetaData();
		assertEquals("length(bean.name)", md.bizQLExpression("name"));
	}

	@Test
	void labelDelegatesToSuperClass() {
		TextLengthBucketMetaData md = new TextLengthBucketMetaData();
		assertEquals("5", md.label(Integer.valueOf(5)));
		assertNull(md.label(null));
	}
}
