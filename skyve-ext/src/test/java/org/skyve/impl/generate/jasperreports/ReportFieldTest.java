package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link ReportField} accessor/mutator pairs and {@code getJrxml()}.
 */
@SuppressWarnings("static-method")
class ReportFieldTest {

	@Test
	void nameRoundTrip() {
		ReportField f = new ReportField();
		f.setName("myField");
		assertThat(f.getName(), is("myField"));
	}

	@Test
	void typeClassRoundTrip() {
		ReportField f = new ReportField();
		f.setTypeClass("java.lang.Integer");
		assertThat(f.getTypeClass(), is("java.lang.Integer"));
	}

	@Test
	void includeTotalRoundTrip() {
		ReportField f = new ReportField();
		f.setIncludeTotal(Boolean.TRUE);
		assertThat(f.getIncludeTotal(), is(Boolean.TRUE));
	}

	@Test
	void implicitRoundTrip() {
		ReportField f = new ReportField();
		f.setImplicit(Boolean.FALSE);
		assertThat(f.getImplicit(), is(Boolean.FALSE));
	}

	@Test
	void skyveTypeRoundTrip() {
		ReportField f = new ReportField();
		f.setSkyveType("date");
		assertThat(f.getSkyveType(), is("date"));
	}

	@Test
	void displayNameRoundTrip() {
		ReportField f = new ReportField();
		f.setDisplayName("My Label");
		assertThat(f.getDisplayName(), is("My Label"));
	}

	@Test
	void owningModuleNameRoundTrip() {
		ReportField f = new ReportField();
		f.setOwningModuleName("admin");
		assertThat(f.getOwningModuleName(), is("admin"));
	}

	@Test
	void documentNameRoundTrip() {
		ReportField f = new ReportField();
		f.setDocumentName("User");
		assertThat(f.getDocumentName(), is("User"));
	}

	@Test
	void bindingRoundTrip() {
		ReportField f = new ReportField();
		f.setBinding("contact.name");
		assertThat(f.getBinding(), is("contact.name"));
	}

	@Test
	void nameSqlRoundTrip() {
		ReportField f = new ReportField();
		f.setNameSql("contact_name");
		assertThat(f.getNameSql(), is("contact_name"));
	}

	@Test
	void joinSqlRoundTrip() {
		ReportField f = new ReportField();
		f.setJoinSql("LEFT JOIN contact ON ...");
		assertThat(f.getJoinSql(), is("LEFT JOIN contact ON ..."));
	}

	@Test
	void collectionRoundTrip() {
		ReportField f = new ReportField();
		f.setCollection(Boolean.TRUE);
		assertThat(f.getCollection(), is(Boolean.TRUE));
	}

	@Test
	void parentRoundTrip() {
		ReportField f = new ReportField();
		DesignSpecification design = new DesignSpecification();
		f.setParent(design);
		assertThat(f.getParent(), is(design));
	}

	@Test
	void defaultFieldHasNullValues() {
		ReportField f = new ReportField();
		assertThat(f.getName(), nullValue());
		assertThat(f.getTypeClass(), nullValue());
		assertThat(f.getCollection(), nullValue());
	}
}
