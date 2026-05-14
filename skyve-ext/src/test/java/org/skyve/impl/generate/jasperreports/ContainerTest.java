package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.jasperreports.Container.ContainerType;

@SuppressWarnings({"static-method", "boxing"})
public class ContainerTest {

	@Test
	public void defaultConstructorSetsDefaults() {
		Container c = new Container();
		assertThat(c.getHorizontal(), is(Boolean.FALSE));
		assertThat(c.getElements(), notNullValue());
		assertTrue(c.getElements().isEmpty());
		assertThat(c.getContainers(), notNullValue());
		assertTrue(c.getContainers().isEmpty());
		assertThat(c.getParent(), nullValue());
	}

	@Test
	public void parameterisedConstructorStoresValues() {
		Container c = new Container(10, 5, 200, true, ContainerType.hbox);
		assertThat(c.getTop(), is(Integer.valueOf(10)));
		assertThat(c.getLeft(), is(Integer.valueOf(5)));
		assertThat(c.getWidth(), is(Integer.valueOf(200)));
		assertThat(c.getHorizontal(), is(Boolean.TRUE));
		assertThat(c.getContainerType(), is(ContainerType.hbox));
	}

	@Test
	public void containerTypeEnumValues() {
		assertThat(ContainerType.tab, notNullValue());
		assertThat(ContainerType.hbox, notNullValue());
		assertThat(ContainerType.vbox, notNullValue());
		assertThat(ContainerType.form, notNullValue());
		assertThat(ContainerType.column, notNullValue());
		assertThat(ContainerType.subreport, notNullValue());
	}

	@Test
	public void borderRoundTrip() {
		Container c = new Container();
		c.setBorder(Boolean.TRUE);
		assertThat(c.getBorder(), is(Boolean.TRUE));
	}

	@Test
	public void addHeightFromNullInitialises() {
		Container c = new Container();
		c.addHeight(Integer.valueOf(15));
		assertThat(c.getHeight(), is(Integer.valueOf(15)));
	}

	@Test
	public void addHeightAccumulates() {
		Container c = new Container();
		c.setHeight(Integer.valueOf(10));
		c.addHeight(Integer.valueOf(5));
		assertThat(c.getHeight(), is(Integer.valueOf(15)));
	}

	@Test
	public void addHeightWithNullValueLeavesHeightUnchanged() {
		Container c = new Container();
		c.setHeight(Integer.valueOf(10));
		c.addHeight(null);
		assertThat(c.getHeight(), is(Integer.valueOf(10)));
	}

	@Test
	public void addWidthFromNullInitialises() {
		Container c = new Container();
		c.addWidth(Integer.valueOf(100));
		assertThat(c.getWidth(), is(Integer.valueOf(100)));
	}

	@Test
	public void addWidthAccumulates() {
		Container c = new Container();
		c.setWidth(Integer.valueOf(80));
		c.addWidth(Integer.valueOf(20));
		assertThat(c.getWidth(), is(Integer.valueOf(100)));
	}

	@Test
	public void addLeftFromNullInitialises() {
		Container c = new Container();
		c.addLeft(Integer.valueOf(5));
		assertThat(c.getLeft(), is(Integer.valueOf(5)));
	}

	@Test
	public void addLeftAccumulates() {
		Container c = new Container();
		c.setLeft(Integer.valueOf(10));
		c.addLeft(Integer.valueOf(7));
		assertThat(c.getLeft(), is(Integer.valueOf(17)));
	}

	@Test
	public void heightRoundTrip() {
		Container c = new Container();
		c.setHeight(Integer.valueOf(42));
		assertThat(c.getHeight(), is(Integer.valueOf(42)));
	}

	@Test
	public void rowsRoundTrip() {
		Container c = new Container();
		c.setRows(3);
		assertThat(c.getRows(), is(3));
	}

	@Test
	public void pixelWidthRoundTrip() {
		Container c = new Container();
		c.setPixelWidth(Integer.valueOf(200));
		assertThat(c.getPixelWidth(), is(Integer.valueOf(200)));
	}

	@Test
	public void percentageWidthRoundTrip() {
		Container c = new Container();
		c.setPercentageWidth(Integer.valueOf(50));
		assertThat(c.getPercentageWidth(), is(Integer.valueOf(50)));
	}

	@Test
	public void responsiveWidthRoundTrip() {
		Container c = new Container();
		c.setResponsiveWidth(Integer.valueOf(6));
		assertThat(c.getResponsiveWidth(), is(Integer.valueOf(6)));
	}

	@Test
	public void filledRoundTrip() {
		Container c = new Container();
		c.setFilled(true);
		assertThat(c.isFilled(), is(true));
	}

	@Test
	public void verticalPositionRoundTrip() {
		Container c = new Container();
		c.setVerticalPosition(7);
		assertThat(c.getVerticalPosition(), is(7));
	}

	@Test
	public void printWhenExpressionRoundTrip() {
		Container c = new Container();
		c.setPrintWhenExpression("$F{active}");
		assertThat(c.getPrintWhenExpression(), is("$F{active}"));
	}

	@Test
	public void borderTitleRoundTrip() {
		Container c = new Container();
		c.setBorderTitle("My Title");
		assertThat(c.getBorderTitle(), is("My Title"));
	}
}
