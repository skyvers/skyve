package org.skyve.metadata.user;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class UserAccessTest {

	// ---- Factory methods and type checks ----

	@Test
	void singularIsSingular() {
		UserAccess ua = UserAccess.singular("mod", "doc");
		assertThat(ua.isSingular(), is(true));
	}

	@Test
	void singularIsNotDocumentAggregate() {
		UserAccess ua = UserAccess.singular("mod", "doc");
		assertThat(ua.isDocumentAggregate(), is(false));
	}

	@Test
	void singularModuleName() {
		UserAccess ua = UserAccess.singular("myMod", "myDoc");
		assertThat(ua.getModuleName(), is("myMod"));
	}

	@Test
	void singularDocumentName() {
		UserAccess ua = UserAccess.singular("myMod", "myDoc");
		assertThat(ua.getDocumentName(), is("myDoc"));
	}

	@Test
	void singularComponentIsNull() {
		UserAccess ua = UserAccess.singular("mod", "doc");
		assertThat(ua.getComponent(), is(nullValue()));
	}

	@Test
	void documentAggregateIsDocumentAggregate() {
		UserAccess ua = UserAccess.documentAggregate("mod", "doc");
		assertThat(ua.isDocumentAggregate(), is(true));
	}

	@Test
	void documentAggregateDocumentNameIsNull() {
		// documentAggregate stores doc in component not documentName
		UserAccess ua = UserAccess.documentAggregate("mod", "doc");
		assertThat(ua.getDocumentName(), is(nullValue()));
	}

	@Test
	void documentAggregateComponentEqualsDoc() {
		UserAccess ua = UserAccess.documentAggregate("mod", "myDoc");
		assertThat(ua.getComponent(), is("myDoc"));
	}

	@Test
	void queryAggregateIsQueryAggregate() {
		UserAccess ua = UserAccess.queryAggregate("mod", "qry");
		assertThat(ua.isQueryAggregate(), is(true));
	}

	@Test
	void queryAggregateComponentEqualsQueryName() {
		UserAccess ua = UserAccess.queryAggregate("mod", "qryName");
		assertThat(ua.getComponent(), is("qryName"));
	}

	@Test
	void modelAggregateIsModelAggregate() {
		UserAccess ua = UserAccess.modelAggregate("mod", "doc", "modelName");
		assertThat(ua.isModelAggregate(), is(true));
	}

	@Test
	void modelAggregateDocumentName() {
		UserAccess ua = UserAccess.modelAggregate("mod", "doc", "modelName");
		assertThat(ua.getDocumentName(), is("doc"));
	}

	@Test
	void modelAggregateComponentEqualsModelName() {
		UserAccess ua = UserAccess.modelAggregate("mod", "doc", "modelName");
		assertThat(ua.getComponent(), is("modelName"));
	}

	@Test
	void previousCompleteIsPreviousComplete() {
		UserAccess ua = UserAccess.previousComplete("mod", "doc", "binding");
		assertThat(ua.isPreviousComplete(), is(true));
	}

	@Test
	void previousCompleteComponent() {
		UserAccess ua = UserAccess.previousComplete("mod", "doc", "myBinding");
		assertThat(ua.getComponent(), is("myBinding"));
	}

	@Test
	void reportIsReport() {
		UserAccess ua = UserAccess.report("mod", "doc", "reportName");
		assertThat(ua.isReport(), is(true));
	}

	@Test
	void reportComponent() {
		UserAccess ua = UserAccess.report("mod", "doc", "rpt");
		assertThat(ua.getComponent(), is("rpt"));
	}

	@Test
	void dynamicImageIsDynamicImage() {
		UserAccess ua = UserAccess.dynamicImage("mod", "doc", "imgName");
		assertThat(ua.isDynamicImage(), is(true));
	}

	@Test
	void contentIsContent() {
		UserAccess ua = UserAccess.content("mod", "doc", "binding");
		assertThat(ua.isContent(), is(true));
	}

	@Test
	void contentComponent() {
		UserAccess ua = UserAccess.content("mod", "doc", "attachment");
		assertThat(ua.getComponent(), is("attachment"));
	}

	// ---- toString ----

	@Test
	void singularToStringNotNull() {
		UserAccess ua = UserAccess.singular("mod", "doc");
		assertThat(ua.toString(), is(notNullValue()));
	}

	@Test
	void singularToStringContainsModuleAndDoc() {
		UserAccess ua = UserAccess.singular("myMod", "myDoc");
		String s = ua.toString();
		assertThat(s.contains("myMod"), is(true));
		assertThat(s.contains("myDoc"), is(true));
	}

	@Test
	void singularToStringStartsWithS() {
		UserAccess ua = UserAccess.singular("mod", "doc");
		assertThat(ua.toString().startsWith("S"), is(true));
	}

	@Test
	void documentAggregateToStringStartsWithD() {
		UserAccess ua = UserAccess.documentAggregate("mod", "doc");
		assertThat(ua.toString().startsWith("D"), is(true));
	}

	// ---- equals / hashCode / compareTo ----

	@Test
	void equalsSameValues() {
		UserAccess ua1 = UserAccess.singular("mod", "doc");
		UserAccess ua2 = UserAccess.singular("mod", "doc");
		assertThat(ua1.equals(ua2), is(true));
	}

	@Test
	void notEqualsDifferentType() {
		UserAccess ua1 = UserAccess.singular("mod", "doc");
		UserAccess ua2 = UserAccess.documentAggregate("mod", "doc");
		assertThat(ua1.equals(ua2), is(false));
	}

	@Test
	void notEqualsNull() {
		UserAccess ua = UserAccess.singular("mod", "doc");
		assertThat(ua.equals(null), is(false));
	}

	@Test
	void hashCodeConsistentWithEquals() {
		UserAccess ua1 = UserAccess.singular("mod", "doc");
		UserAccess ua2 = UserAccess.singular("mod", "doc");
		assertThat(ua1.hashCode(), is(ua2.hashCode()));
	}

	@Test
	void compareToSameReturnsZero() {
		UserAccess ua1 = UserAccess.singular("mod", "doc");
		UserAccess ua2 = UserAccess.singular("mod", "doc");
		assertThat(ua1.compareTo(ua2), is(0));
	}

	@Test
	void compareToNotSameReturnsNonZero() {
		UserAccess ua1 = UserAccess.singular("mod", "doc");
		UserAccess ua2 = UserAccess.queryAggregate("mod", "qry");
		assertThat(ua1.compareTo(ua2) == 0, is(false));
	}

	// ---- ALL_UX_UIS ----

	@Test
	void allUxUisIsEmpty() {
		assertThat(UserAccess.ALL_UX_UIS.isEmpty(), is(true));
	}

	// ---- additional false-path coverage ----

	@Test
	void queryAggregateIsNotSingular() {
		assertThat(UserAccess.queryAggregate("m", "q").isSingular(), is(false));
	}

	@Test
	void singularIsNotQueryAggregate() {
		assertThat(UserAccess.singular("m", "d").isQueryAggregate(), is(false));
	}

	@Test
	void singularIsNotModelAggregate() {
		assertThat(UserAccess.singular("m", "d").isModelAggregate(), is(false));
	}

	@Test
	void singularIsNotPreviousComplete() {
		assertThat(UserAccess.singular("m", "d").isPreviousComplete(), is(false));
	}

	@Test
	void singularIsNotReport() {
		assertThat(UserAccess.singular("m", "d").isReport(), is(false));
	}

	@Test
	void singularIsNotDynamicImage() {
		assertThat(UserAccess.singular("m", "d").isDynamicImage(), is(false));
	}

	@Test
	void singularIsNotContent() {
		assertThat(UserAccess.singular("m", "d").isContent(), is(false));
	}

	@Test
	void reportToStringContainsModuleDocumentAndComponent() {
		UserAccess ua = UserAccess.report("mod", "doc", "myReport");
		String s = ua.toString();
		assertTrue(s.contains("mod"));
		assertTrue(s.contains("doc"));
		assertTrue(s.contains("myReport"));
	}

	@Test
	void dynamicImageToStringContainsModuleDocumentAndImage() {
		UserAccess ua = UserAccess.dynamicImage("mod", "doc", "imgName");
		String s = ua.toString();
		assertTrue(s.contains("mod"));
		assertTrue(s.contains("doc"));
		assertTrue(s.contains("imgName"));
	}

	@Test
	void contentToStringContainsModuleDocumentAndBinding() {
		UserAccess ua = UserAccess.content("mod", "doc", "attachment");
		String s = ua.toString();
		assertTrue(s.contains("mod"));
		assertTrue(s.contains("doc"));
		assertTrue(s.contains("attachment"));
	}

	@Test
	void modelAggregateToStringContainsModuleDocumentAndModel() {
		UserAccess ua = UserAccess.modelAggregate("mod", "doc", "MyModel");
		String s = ua.toString();
		assertTrue(s.contains("mod"));
		assertTrue(s.contains("doc"));
		assertTrue(s.contains("MyModel"));
	}

	@Test
	void singularToStringContainsModuleAndDocument() {
		UserAccess ua = UserAccess.singular("mod", "doc");
		String s = ua.toString();
		assertTrue(s.contains("mod"));
		assertTrue(s.contains("doc"));
	}

	@Test
	void equalsReturnsTrueForSameValues() {
		UserAccess a = UserAccess.singular("mod", "doc");
		UserAccess b = UserAccess.singular("mod", "doc");
		assertEquals(a, b);
	}

	@Test
	void equalsReturnsFalseForDifferentValues() {
		UserAccess a = UserAccess.singular("mod", "doc");
		UserAccess b = UserAccess.singular("mod", "other");
		assertNotEquals(a, b);
	}

	@Test
	void equalsReturnsFalseForNull() {
		UserAccess a = UserAccess.singular("mod", "doc");
		assertFalse(a.equals(null));
	}

	@Test
	void hashCodeIsConsistentWithEquals() {
		UserAccess a = UserAccess.singular("mod", "doc");
		UserAccess b = UserAccess.singular("mod", "doc");
		assertEquals(a.hashCode(), b.hashCode());
	}

	@Test
	void compareToReturnsZeroForEqual() {
		UserAccess a = UserAccess.singular("mod", "doc");
		UserAccess b = UserAccess.singular("mod", "doc");
		assertEquals(0, a.compareTo(b));
	}

	@Test
	void compareToReturnsNonZeroForDifferent() {
		UserAccess a = UserAccess.singular("aaa", "doc");
		UserAccess b = UserAccess.singular("zzz", "doc");
		assertTrue(a.compareTo(b) < 0);
		assertTrue(b.compareTo(a) > 0);
	}
}
