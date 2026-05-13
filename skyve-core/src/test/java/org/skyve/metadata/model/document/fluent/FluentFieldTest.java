package org.skyve.metadata.model.document.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.field.Field.GeneratedType;

class FluentFieldTest {
	@Test
	@SuppressWarnings("static-method")
	void testGeneratedDefaultsToNull() {
		FluentInteger fluent = new FluentInteger();
		assertThat(fluent.get().getGenerated(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetGeneratedInsert() {
		FluentInteger fluent = new FluentInteger();
		fluent.generated(GeneratedType.insert);
		assertThat(fluent.get().getGenerated(), is(GeneratedType.insert));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetGeneratedAlways() {
		FluentInteger fluent = new FluentInteger();
		fluent.generated(GeneratedType.always);
		assertThat(fluent.get().getGenerated(), is(GeneratedType.always));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetGeneratedNullClearsValue() {
		FluentInteger fluent = new FluentInteger();
		fluent.generated(GeneratedType.insert);
		fluent.generated(null);
		assertThat(fluent.get().getGenerated(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromCopiesGenerated() {
		org.skyve.impl.metadata.model.document.field.Integer source =
				new org.skyve.impl.metadata.model.document.field.Integer();
		source.setName("count");
		source.setDisplayName("Count");
		source.setGenerated(GeneratedType.insert);

		FluentInteger fluent = new FluentInteger(source);
		FluentInteger copy = new FluentInteger().from(source);

		assertThat(copy.get().getGenerated(), is(GeneratedType.insert));
		assertThat(fluent.get().getGenerated(), is(GeneratedType.insert));
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromOmitsGeneratedWhenNull() {
		org.skyve.impl.metadata.model.document.field.Integer source =
				new org.skyve.impl.metadata.model.document.field.Integer();
		source.setName("count");
		source.setDisplayName("Count");
		// generated not set

		FluentInteger copy = new FluentInteger().from(source);
		assertThat(copy.get().getGenerated(), is(nullValue()));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	void testFluentChainReturnsSelf() {
		FluentInteger fluent = new FluentInteger();
		FluentInteger returned = fluent.generated(GeneratedType.always);
		assertThat(returned == fluent, is(true));
	}
}
