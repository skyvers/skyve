package org.skyve.metadata.module.fluent;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class FluentListItemTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentListItem().get(), is(notNullValue()));
	}

	@Test
	void documentNameSetsValue() {
		assertThat(new FluentListItem().documentName("Contact").get().getDocumentName(), is("Contact"));
	}

	@Test
	void queryNameSetsValue() {
		assertThat(new FluentListItem().queryName("qContacts").get().getQueryName(), is("qContacts"));
	}

	@Test
	void modelNameSetsValue() {
		assertThat(new FluentListItem().modelName("myModel").get().getModelName(), is("myModel"));
	}

	@Test
	void autoPopulateSetsTrue() {
		assertThat(new FluentListItem().autoPopulate(true).get().getAutoPopulate(), is(Boolean.TRUE));
	}

	@Test
	void autoPopulateSetsFalse() {
		assertThat(new FluentListItem().autoPopulate(false).get().getAutoPopulate(), is(Boolean.FALSE));
	}
}
