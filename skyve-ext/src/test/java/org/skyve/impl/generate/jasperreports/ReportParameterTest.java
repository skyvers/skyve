package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ReportParameterTest {

	@Test
	void nameRoundTrip() {
		ReportParameter p = new ReportParameter();
		p.setName("myParam");
		assertThat(p.getName(), is("myParam"));
	}

	@Test
	void typeClassRoundTrip() {
		ReportParameter p = new ReportParameter();
		p.setTypeClass("java.lang.String");
		assertThat(p.getTypeClass(), is("java.lang.String"));
	}

	@Test
	void defaultValueExpressionRoundTrip() {
		ReportParameter p = new ReportParameter();
		p.setDefaultValueExpression("\"default\"");
		assertThat(p.getDefaultValueExpression(), is("\"default\""));
	}

	@Test
	void parentRoundTrip() {
		ReportParameter p = new ReportParameter();
		DesignSpecification ds = new DesignSpecification();
		p.setParent(ds);
		assertThat(p.getParent(), is(ds));
	}

	@Test
	void defaultConstructorHasNullValues() {
		ReportParameter p = new ReportParameter();
		assertThat(p.getName(), nullValue());
		assertThat(p.getTypeClass(), nullValue());
		assertThat(p.getDefaultValueExpression(), nullValue());
		assertThat(p.getParent(), nullValue());
	}

	@Test
	void getJrxmlReturnsXmlWithParameterName() {
		ReportParameter p = new ReportParameter();
		p.setName("testParam");
		p.setTypeClass("java.lang.String");
		assertThat(p.getJrxml(), notNullValue());
		assertThat(p.getJrxml(), containsString("testParam"));
	}

	@Test
	void getJrxmlWithNullDefaultValueExpressionEmitsCDATA() {
		ReportParameter p = new ReportParameter();
		p.setName("p1");
		p.setTypeClass("java.lang.Integer");
		assertThat(p.getJrxml(), containsString("<![CDATA[]]>"));
	}
}
