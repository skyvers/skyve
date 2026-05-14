package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class ReportFieldParameterVariableTest {
	private DesignSpecification parent;

	@BeforeEach
	public void setUp() {
		parent = new DesignSpecification();
		parent.setModuleName("test");
		parent.setDocumentName("AllAttributesPersistent");
		parent.setMode(DesignSpecification.Mode.bean);
	}

	// --- ReportField ---

	@Test
	public void fieldNameRoundTrip() {
		ReportField f = new ReportField();
		f.setName("myField");
		assertThat(f.getName(), is("myField"));
	}

	@Test
	public void fieldTypeClassRoundTrip() {
		ReportField f = new ReportField();
		f.setTypeClass("java.lang.String");
		assertThat(f.getTypeClass(), is("java.lang.String"));
	}

	@Test
	public void fieldJrxmlIsNonNullWhenParentSet() {
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
	public void parameterNameRoundTrip() {
		ReportParameter p = new ReportParameter();
		p.setName("myParam");
		assertThat(p.getName(), is("myParam"));
	}

	@Test
	public void parameterTypeClassRoundTrip() {
		ReportParameter p = new ReportParameter();
		p.setTypeClass("java.lang.String");
		assertThat(p.getTypeClass(), is("java.lang.String"));
	}

	@Test
	public void parameterJrxmlIsNonNullWhenParentSet() {
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
	public void variableNameRoundTrip() {
		ReportVariable v = new ReportVariable();
		v.setName("myVar");
		assertThat(v.getName(), is("myVar"));
	}

	@Test
	public void variableTypeClassRoundTrip() {
		ReportVariable v = new ReportVariable();
		v.setTypeClass("java.lang.Integer");
		assertThat(v.getTypeClass(), is("java.lang.Integer"));
	}

	@Test
	public void variableJrxmlIsNonNullWhenParentSet() {
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
	public void variableValueRoundTrip() {
		ReportVariable v = new ReportVariable();
		v.setValue("some expression");
		assertThat(v.getValue(), is("some expression"));
	}

	@Test
	public void variableParentRoundTrip() {
		ReportVariable v = new ReportVariable();
		v.setParent(parent);
		assertThat(v.getParent(), is(parent));
	}

	@Test
	public void variableJrxmlContainsIncrementTypeColumn() {
		ReportVariable v = new ReportVariable();
		v.setName("total");
		v.setTypeClass("java.lang.Long");
		v.setParent(parent);
		assertThat(v.getJrxml(), containsString("Column"));
	}

	@Test
	public void variableJrxmlContainsVariableAndInitialValueExpression() {
		ReportVariable v = new ReportVariable();
		v.setName("count");
		v.setTypeClass("java.lang.Integer");
		v.setParent(parent);
		String jrxml = v.getJrxml();
		assertThat(jrxml, containsString("variableExpression"));
		assertThat(jrxml, containsString("initialValueExpression"));
	}
}
