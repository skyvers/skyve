package org.skyve.metadata.view.fluent;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;

@SuppressWarnings("static-method")
class FluentLookupDescriptionTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentLookupDescription().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorUsesInstance() {
		LookupDescription ld = new LookupDescription();
		assertEquals(ld, new FluentLookupDescription(ld).get());
	}

	@Test
	void descriptionBindingSetsValue() {
		assertThat(new FluentLookupDescription().descriptionBinding("name").get().getDescriptionBinding(), is("name"));
	}

	@Test
	void querySetsValue() {
		assertThat(new FluentLookupDescription().query("qContacts").get().getQuery(), is("qContacts"));
	}

	@Test
	void disableEditConditionNameSetsValue() {
		assertThat(new FluentLookupDescription().disableEditConditionName("noEdit").get().getDisableEditConditionName(), is("noEdit"));
	}

	@Test
	void disableAddConditionNameSetsValue() {
		assertThat(new FluentLookupDescription().disableAddConditionName("noAdd").get().getDisableAddConditionName(), is("noAdd"));
	}

	@Test
	void disableClearConditionNameSetsValue() {
		assertThat(new FluentLookupDescription().disableClearConditionName("noClear").get().getDisableClearConditionName(), is("noClear"));
	}

	@Test
	void disablePickConditionNameSetsValue() {
		assertThat(new FluentLookupDescription().disablePickConditionName("noPick").get().getDisablePickConditionName(), is("noPick"));
	}

	@Test
	void editableSetsTrue() {
		assertThat(new FluentLookupDescription().editable(true).get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	void editableSetsFalse() {
		assertThat(new FluentLookupDescription().editable(false).get().getEditable(), is(Boolean.FALSE));
	}

	@Test
	void pixelWidthSetsValue() {
		assertEquals(300, new FluentLookupDescription().pixelWidth(300).get().getPixelWidth().intValue());
	}

	@Test
	void addDropDownColumnAddsColumn() {
		FluentLookupDescription ld = new FluentLookupDescription().addDropDownColumn(new FluentLookupDescriptionColumn());
		assertEquals(1, ld.get().getDropDownColumns().size());
	}

	@Test
	void addEditedActionAddsAction() {
		FluentLookupDescription ld = new FluentLookupDescription().addEditedAction(new FluentRerenderEventAction());
		assertEquals(1, ld.get().getEditedActions().size());
	}

	@Test
	void addAddedActionAddsAction() {
		FluentLookupDescription ld = new FluentLookupDescription().addAddedAction(new FluentRerenderEventAction());
		assertEquals(1, ld.get().getAddedActions().size());
	}

	@Test
	void addClearedActionAddsAction() {
		FluentLookupDescription ld = new FluentLookupDescription().addClearedAction(new FluentRerenderEventAction());
		assertEquals(1, ld.get().getClearedActions().size());
	}

	@Test
	void addPickedActionAddsAction() {
		FluentLookupDescription ld = new FluentLookupDescription().addPickedAction(new FluentRerenderEventAction());
		assertEquals(1, ld.get().getPickedActions().size());
	}

	@Test
	void addFilterParameterAddsParam() {
		FluentLookupDescription ld = new FluentLookupDescription().addFilterParameter(new FluentFilterParameter());
		assertEquals(1, ld.get().getFilterParameters().size());
	}

	@Test
	void addParameterAddsParam() {
		FluentLookupDescription ld = new FluentLookupDescription().addParameter(new FluentParameter());
		assertEquals(1, ld.get().getParameters().size());
	}

	@Test
	void fromCopiesDescriptionBinding() {
		LookupDescription src = new LookupDescription();
		src.setDescriptionBinding("name");
		assertThat(new FluentLookupDescription().from(src).get().getDescriptionBinding(), is("name"));
	}

	@Test
	void fromCopiesQuery() {
		LookupDescription src = new LookupDescription();
		src.setQuery("qContacts");
		assertThat(new FluentLookupDescription().from(src).get().getQuery(), is("qContacts"));
	}

	@Test
	void fromCopiesEditable() {
		LookupDescription src = new LookupDescription();
		src.setEditable(Boolean.TRUE);
		assertThat(new FluentLookupDescription().from(src).get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesDropDownColumns() {
		LookupDescription src = new LookupDescription();
		src.getDropDownColumns().add(new FluentLookupDescriptionColumn().get());
		assertEquals(1, new FluentLookupDescription().from(src).get().getDropDownColumns().size());
	}

	@Test
	void fromCopiesAddedActions() {
		LookupDescription src = new LookupDescription();
		src.getAddedActions().add(new FluentRerenderEventAction().get());
		assertEquals(1, new FluentLookupDescription().from(src).get().getAddedActions().size());
	}

	@Test
	void fromCopiesFilterParameters() {
		LookupDescription src = new LookupDescription();
		src.getFilterParameters().add(new FluentFilterParameter().get());
		assertEquals(1, new FluentLookupDescription().from(src).get().getFilterParameters().size());
	}

	@Test
	void fromCopiesClearedActions() {
		LookupDescription src = new LookupDescription();
		src.getClearedActions().add(new FluentRerenderEventAction().get());
		assertEquals(1, new FluentLookupDescription().from(src).get().getClearedActions().size());
	}

	@Test
	void fromCopiesEditedActions() {
		LookupDescription src = new LookupDescription();
		src.getEditedActions().add(new FluentRerenderEventAction().get());
		assertEquals(1, new FluentLookupDescription().from(src).get().getEditedActions().size());
	}

	@Test
	void fromCopiesPickedActions() {
		LookupDescription src = new LookupDescription();
		src.getPickedActions().add(new FluentRerenderEventAction().get());
		assertEquals(1, new FluentLookupDescription().from(src).get().getPickedActions().size());
	}

	@Test
	void fromCopiesParametersLambda() {
		LookupDescription src = new LookupDescription();
		ParameterImpl p = new ParameterImpl();
		p.setName("lookupParam");
		src.getParameters().add(p);
		assertEquals(1, new FluentLookupDescription().from(src).get().getParameters().size());
	}
}
