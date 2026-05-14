package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
@SuppressWarnings("static-method")
public class ViewReportDesignGeneratorTest {

	@Mock
	private ReportViewVisitor mockVisitor;

	@Test
	public void defaultConstructorCreatesInstance() {
		assertThat(new ViewReportDesignGenerator(), notNullValue());
	}

	@Test
	public void defaultConstructorHasNullVisitor() {
		assertThat(new ViewReportDesignGenerator().getVisitor(), nullValue());
	}

	@Test
	public void constructorWithVisitorStoresVisitor() {
		ViewReportDesignGenerator gen = new ViewReportDesignGenerator(mockVisitor);
		assertThat(gen.getVisitor(), notNullValue());
	}

	@Test
	public void setVisitorStoresValue() {
		ViewReportDesignGenerator gen = new ViewReportDesignGenerator();
		gen.setVisitor(mockVisitor);
		assertThat(gen.getVisitor(), notNullValue());
	}

	@Test
	public void getSubreportGeneratorReturnsViewReportDesignGeneratorInstance() {
		ViewReportDesignGenerator gen = new ViewReportDesignGenerator();
		assertThat(gen.getSubreportGenerator(), instanceOf(ViewReportDesignGenerator.class));
	}
}
