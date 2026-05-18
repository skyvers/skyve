package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ReportFieldParameterVariableTest {
	private DesignSpecification parent;

	@BeforeEach
	void setUp() {
		parent = new DesignSpecification();
		parent.setModuleName("test");
		parent.setDocumentName("AllAttributesPersistent");
		parent.setMode(DesignSpecification.Mode.bean);
	}

	// --- ReportField ---

	@Test
	void fieldNameRoundTrip() {
		ReportField f = new ReportField();
		f.setName("myField");
		assertThat(f.getName(), is("myField"));
	}

	@Test
	void fieldTypeClassRoundTrip() {
		ReportField f = new ReportField();
		f.setTypeClass("java.lang.String");
		assertThat(f.getTypeClass(), is("java.lang.String"));
	}

	@Test
	void fieldJrxmlIsNonNullWhenParentSet() {
		ReportField f = new ReportField();
		f.setName("testField");
		f.setTypeClass("java.lang.String");
		f.setParent(parent);

		String jrxml = f.getJrxml();
		assertThat(jrxml, notNullValue());
		assertFalse(jrxml.isEmpty());
		assertThat(jrxml, containsString("testField"));
		assertThat(jrxml, containsString("java.lang.String"));
	}

	// --- ReportParameter ---

	@Test
	void parameterNameRoundTrip() {
		ReportParameter p = new ReportParameter();
		p.setName("myParam");
		assertThat(p.getName(), is("myParam"));
	}

	@Test
	void parameterTypeClassRoundTrip() {
		ReportParameter p = new ReportParameter();
		p.setTypeClass("java.lang.String");
		assertThat(p.getTypeClass(), is("java.lang.String"));
	}

	@Test
	void parameterJrxmlIsNonNullWhenParentSet() {
		ReportParameter p = new ReportParameter();
		p.setName("ID");
		p.setTypeClass("java.lang.String");
		p.setParent(parent);

		String jrxml = p.getJrxml();
		assertThat(jrxml, notNullValue());
		assertFalse(jrxml.isEmpty());
		assertThat(jrxml, containsString("ID"));
		assertThat(jrxml, containsString("java.lang.String"));
	}

	// --- ReportVariable ---

	@Test
	void variableNameRoundTrip() {
		ReportVariable v = new ReportVariable();
		v.setName("myVar");
		assertThat(v.getName(), is("myVar"));
	}

	@Test
	void variableTypeClassRoundTrip() {
		ReportVariable v = new ReportVariable();
		v.setTypeClass("java.lang.Integer");
		assertThat(v.getTypeClass(), is("java.lang.Integer"));
	}

	@Test
	void variableJrxmlIsNonNullWhenParentSet() {
		ReportVariable v = new ReportVariable();
		v.setName("totalCount");
		v.setTypeClass("java.lang.Integer");
		v.setParent(parent);

		String jrxml = v.getJrxml();
		assertThat(jrxml, notNullValue());
		assertFalse(jrxml.isEmpty());
		assertThat(jrxml, containsString("totalCount"));
	}

	@Test
	void variableValueRoundTrip() {
		ReportVariable v = new ReportVariable();
		v.setValue("some expression");
		assertThat(v.getValue(), is("some expression"));
	}

	@Test
	void variableParentRoundTrip() {
		ReportVariable v = new ReportVariable();
		v.setParent(parent);
		assertThat(v.getParent(), is(parent));
	}

	@Test
	void variableJrxmlContainsIncrementTypeColumn() {
		ReportVariable v = new ReportVariable();
		v.setName("total");
		v.setTypeClass("java.lang.Long");
		v.setParent(parent);
		assertThat(v.getJrxml(), containsString("Column"));
	}

	@Test
	void variableJrxmlContainsVariableAndInitialValueExpression() {
		ReportVariable v = new ReportVariable();
		v.setName("count");
		v.setTypeClass("java.lang.Integer");
		v.setParent(parent);
		String jrxml = v.getJrxml();
		assertThat(jrxml, containsString("variableExpression"));
		assertThat(jrxml, containsString("initialValueExpression"));
	}
}
