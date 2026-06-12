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
		assertTrue(ua.isSingular());
	}

	@Test
	void singularIsNotDocumentAggregate() {
		UserAccess ua = UserAccess.singular("mod", "doc");
		assertFalse(ua.isDocumentAggregate());
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
		assertTrue(ua.isDocumentAggregate());
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
		assertTrue(ua.isQueryAggregate());
	}

	@Test
	void queryAggregateComponentEqualsQueryName() {
		UserAccess ua = UserAccess.queryAggregate("mod", "qryName");
		assertThat(ua.getComponent(), is("qryName"));
	}

	@Test
	void modelAggregateIsModelAggregate() {
		UserAccess ua = UserAccess.modelAggregate("mod", "doc", "modelName");
		assertTrue(ua.isModelAggregate());
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
		assertTrue(ua.isPreviousComplete());
	}

	@Test
	void previousCompleteComponent() {
		UserAccess ua = UserAccess.previousComplete("mod", "doc", "myBinding");
		assertThat(ua.getComponent(), is("myBinding"));
	}

	@Test
	void reportIsReport() {
		UserAccess ua = UserAccess.report("mod", "doc", "reportName");
		assertTrue(ua.isReport());
	}

	@Test
	void reportComponent() {
		UserAccess ua = UserAccess.report("mod", "doc", "rpt");
		assertThat(ua.getComponent(), is("rpt"));
	}

	@Test
	void dynamicImageIsDynamicImage() {
		UserAccess ua = UserAccess.dynamicImage("mod", "doc", "imgName");
		assertTrue(ua.isDynamicImage());
	}

	@Test
	void contentIsContent() {
		UserAccess ua = UserAccess.content("mod", "doc", "binding");
		assertTrue(ua.isContent());
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
		assertTrue(s.contains("myMod"));
		assertTrue(s.contains("myDoc"));
	}

	@Test
	void singularToStringStartsWithS() {
		UserAccess ua = UserAccess.singular("mod", "doc");
		assertTrue(ua.toString().startsWith("S"));
	}

	@Test
	void documentAggregateToStringStartsWithD() {
		UserAccess ua = UserAccess.documentAggregate("mod", "doc");
		assertTrue(ua.toString().startsWith("D"));
	}

	// ---- equals / hashCode / compareTo ----

	@Test
	void equalsSameValues() {
		UserAccess ua1 = UserAccess.singular("mod", "doc");
		UserAccess ua2 = UserAccess.singular("mod", "doc");
		assertEquals(ua1, ua2);
	}

	@Test
	void notEqualsDifferentType() {
		UserAccess ua1 = UserAccess.singular("mod", "doc");
		UserAccess ua2 = UserAccess.documentAggregate("mod", "doc");
		assertNotEquals(ua1, ua2);
	}

	@Test
	void notEqualsNull() {
		UserAccess ua = UserAccess.singular("mod", "doc");
		assertNotEquals(null, ua);
	}

	@Test
	void hashCodeConsistentWithEquals() {
		UserAccess ua1 = UserAccess.singular("mod", "doc");
		UserAccess ua2 = UserAccess.singular("mod", "doc");
		assertEquals(ua1.hashCode(), ua2.hashCode());
	}

	@Test
	void compareToSameReturnsZero() {
		UserAccess ua1 = UserAccess.singular("mod", "doc");
		UserAccess ua2 = UserAccess.singular("mod", "doc");
		assertEquals(0, ua1.compareTo(ua2));
	}

	@Test
	void compareToNotSameReturnsNonZero() {
		UserAccess ua1 = UserAccess.singular("mod", "doc");
		UserAccess ua2 = UserAccess.queryAggregate("mod", "qry");
		assertNotEquals(0, ua1.compareTo(ua2));
	}

	// ---- ALL_UX_UIS ----

	@Test
	void allUxUisIsEmpty() {
		assertTrue(UserAccess.ALL_UX_UIS.isEmpty());
	}

	// ---- additional false-path coverage ----

	@Test
	void queryAggregateIsNotSingular() {
		assertFalse(UserAccess.queryAggregate("m", "q").isSingular());
	}

	@Test
	void singularIsNotQueryAggregate() {
		assertFalse(UserAccess.singular("m", "d").isQueryAggregate());
	}

	@Test
	void singularIsNotModelAggregate() {
		assertFalse(UserAccess.singular("m", "d").isModelAggregate());
	}

	@Test
	void singularIsNotPreviousComplete() {
		assertFalse(UserAccess.singular("m", "d").isPreviousComplete());
	}

	@Test
	void singularIsNotReport() {
		assertFalse(UserAccess.singular("m", "d").isReport());
	}

	@Test
	void singularIsNotDynamicImage() {
		assertFalse(UserAccess.singular("m", "d").isDynamicImage());
	}

	@Test
	void singularIsNotContent() {
		assertFalse(UserAccess.singular("m", "d").isContent());
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
		assertNotEquals(a, null);
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
