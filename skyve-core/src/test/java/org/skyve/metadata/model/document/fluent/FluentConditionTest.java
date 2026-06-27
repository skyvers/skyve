package org.skyve.metadata.model.document.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Attribute.UsageType;

/**
 * Tests for {@link FluentCondition} setters and {@code from()}.
 */
@SuppressWarnings("static-method")
class FluentConditionTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new FluentCondition().get());
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		org.skyve.impl.metadata.repository.document.ConditionMetaData c =
				new org.skyve.impl.metadata.repository.document.ConditionMetaData();
		FluentCondition fc = new FluentCondition(c);
		assertEquals(c, fc.get());
	}

	@Test
	void nameSetsValue() {
		FluentCondition fc = new FluentCondition().name("myCondition");
		assertEquals("myCondition", fc.get().getName());
	}

	@Test
	void documentationSetsValue() {
		FluentCondition fc = new FluentCondition().documentation("docs");
		assertEquals("docs", fc.get().getDocumentation());
	}

	@Test
	void descriptionSetsValue() {
		FluentCondition fc = new FluentCondition().description("desc");
		assertEquals("desc", fc.get().getDescription());
	}

	@Test
	void expressionSetsValue() {
		FluentCondition fc = new FluentCondition().expression("bean.active");
		assertEquals("bean.active", fc.get().getExpression());
	}

	@Test
	void usageSetsValue() {
		FluentCondition fc = new FluentCondition().usage(UsageType.view);
		assertEquals(UsageType.view, fc.get().getUsage());
	}

	@Test
	void fromCopiesAllFields() {
		// Build a source Condition via a ConditionMetaData set up manually
		org.skyve.impl.metadata.repository.document.ConditionMetaData source =
				new org.skyve.impl.metadata.repository.document.ConditionMetaData();
		source.setName("srcCond");
		source.setDocumentation("doc1");
		source.setDescription("desc1");
		source.setExpression("isActive()");
		source.setUsage(UsageType.domain);

		FluentCondition copy = new FluentCondition().from("copiedName", source);

		assertEquals("copiedName", copy.get().getName());
		assertEquals("doc1", copy.get().getDocumentation());
		assertEquals("desc1", copy.get().getDescription());
		assertEquals("isActive()", copy.get().getExpression());
		assertEquals(UsageType.domain, copy.get().getUsage());
	}
}
