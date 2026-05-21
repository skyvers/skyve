package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.jasperreports.Container.ContainerType;

@SuppressWarnings({"static-method", "boxing"})
class ContainerTest {

	@Test
	void defaultConstructorSetsDefaults() {
		Container c = new Container();
		assertThat(c.getHorizontal(), is(Boolean.FALSE));
		assertThat(c.getElements(), notNullValue());
		assertTrue(c.getElements().isEmpty());
		assertThat(c.getContainers(), notNullValue());
		assertTrue(c.getContainers().isEmpty());
		assertThat(c.getParent(), nullValue());
	}

	@Test
	void parameterisedConstructorStoresValues() {
		Container c = new Container(10, 5, 200, true, ContainerType.hbox);
		assertThat(c.getTop(), is(Integer.valueOf(10)));
		assertThat(c.getLeft(), is(Integer.valueOf(5)));
		assertThat(c.getWidth(), is(Integer.valueOf(200)));
		assertThat(c.getHorizontal(), is(Boolean.TRUE));
		assertThat(c.getContainerType(), is(ContainerType.hbox));
	}

	@Test
	void containerTypeEnumValues() {
		assertThat(ContainerType.tab, notNullValue());
		assertThat(ContainerType.hbox, notNullValue());
		assertThat(ContainerType.vbox, notNullValue());
		assertThat(ContainerType.form, notNullValue());
		assertThat(ContainerType.column, notNullValue());
		assertThat(ContainerType.subreport, notNullValue());
	}

	@Test
	void borderRoundTrip() {
		Container c = new Container();
		c.setBorder(Boolean.TRUE);
		assertThat(c.getBorder(), is(Boolean.TRUE));
	}

	@Test
	void addHeightFromNullInitialises() {
		Container c = new Container();
		c.addHeight(Integer.valueOf(15));
		assertThat(c.getHeight(), is(Integer.valueOf(15)));
	}

	@Test
	void addHeightAccumulates() {
		Container c = new Container();
		c.setHeight(Integer.valueOf(10));
		c.addHeight(Integer.valueOf(5));
		assertThat(c.getHeight(), is(Integer.valueOf(15)));
	}

	@Test
	void addHeightWithNullValueLeavesHeightUnchanged() {
		Container c = new Container();
		c.setHeight(Integer.valueOf(10));
		c.addHeight(null);
		assertThat(c.getHeight(), is(Integer.valueOf(10)));
	}

	@Test
	void addWidthFromNullInitialises() {
		Container c = new Container();
		c.addWidth(Integer.valueOf(100));
		assertThat(c.getWidth(), is(Integer.valueOf(100)));
	}

	@Test
	void addWidthAccumulates() {
		Container c = new Container();
		c.setWidth(Integer.valueOf(80));
		c.addWidth(Integer.valueOf(20));
		assertThat(c.getWidth(), is(Integer.valueOf(100)));
	}

	@Test
	void addLeftFromNullInitialises() {
		Container c = new Container();
		c.addLeft(Integer.valueOf(5));
		assertThat(c.getLeft(), is(Integer.valueOf(5)));
	}

	@Test
	void addLeftAccumulates() {
		Container c = new Container();
		c.setLeft(Integer.valueOf(10));
		c.addLeft(Integer.valueOf(7));
		assertThat(c.getLeft(), is(Integer.valueOf(17)));
	}

	@Test
	void heightRoundTrip() {
		Container c = new Container();
		c.setHeight(Integer.valueOf(42));
		assertThat(c.getHeight(), is(Integer.valueOf(42)));
	}

	@Test
	void rowsRoundTrip() {
		Container c = new Container();
		c.setRows(3);
		assertEquals(3, c.getRows());
	}

	@Test
	void pixelWidthRoundTrip() {
		Container c = new Container();
		c.setPixelWidth(Integer.valueOf(200));
		assertThat(c.getPixelWidth(), is(Integer.valueOf(200)));
	}

	@Test
	void percentageWidthRoundTrip() {
		Container c = new Container();
		c.setPercentageWidth(Integer.valueOf(50));
		assertThat(c.getPercentageWidth(), is(Integer.valueOf(50)));
	}

	@Test
	void responsiveWidthRoundTrip() {
		Container c = new Container();
		c.setResponsiveWidth(Integer.valueOf(6));
		assertThat(c.getResponsiveWidth(), is(Integer.valueOf(6)));
	}

	@Test
	void filledRoundTrip() {
		Container c = new Container();
		c.setFilled(true);
		assertTrue(c.isFilled());
	}

	@Test
	void verticalPositionRoundTrip() {
		Container c = new Container();
		c.setVerticalPosition(7);
		assertEquals(7, c.getVerticalPosition());
	}

	@Test
	void printWhenExpressionRoundTrip() {
		Container c = new Container();
		c.setPrintWhenExpression("$F{active}");
		assertThat(c.getPrintWhenExpression(), is("$F{active}"));
	}

	@Test
	void borderTitleRoundTrip() {
		Container c = new Container();
		c.setBorderTitle("My Title");
		assertThat(c.getBorderTitle(), is("My Title"));
	}

	@Test
	void setParentUpdatesDepthToParentDepthPlusOne() {
		Container parent = new Container();
		parent.setDepth(2);

		Container child = new Container();
		child.setParent(parent);

		assertThat(child.getDepth(), is(3));
		assertThat(child.getParent(), is(parent));
	}

	@Test
	void addContainerSetsChildDepthAndParentAndAddsToList() {
		Container parent = new Container();
		Container child = new Container();

		parent.addContainer(child);

		assertThat(parent.getContainers().size(), is(1));
		assertThat(child.getParent(), is(parent));
		assertThat(child.getDepth(), is(1)); // parent.depth=0 → child = 0+1
	}

	@Test
	void depthRoundTrip() {
		Container c = new Container();
		c.setDepth(5);
		assertThat(c.getDepth(), is(5));
	}

        @Test
        void addHeightWhenNullSetsValue() {
                Container c = new Container();
                c.addHeight(Integer.valueOf(10));
                assertThat(c.getHeight(), is(Integer.valueOf(10)));
        }

        @Test
        void addHeightWhenSetAccumulates() {
                Container c = new Container();
                c.setHeight(Integer.valueOf(5));
                c.addHeight(Integer.valueOf(3));
                assertThat(c.getHeight(), is(Integer.valueOf(8)));
        }

        @Test
        void addWidthWhenNullSetsValue() {
                Container c = new Container();
                c.addWidth(Integer.valueOf(20));
                assertThat(c.getWidth(), is(Integer.valueOf(20)));
        }

        @Test
        void addWidthWhenSetAccumulates() {
                Container c = new Container();
                c.setWidth(Integer.valueOf(10));
                c.addWidth(Integer.valueOf(5));
                assertThat(c.getWidth(), is(Integer.valueOf(15)));
        }

        @Test
        void addLeftWhenNullSetsValue() {
                Container c = new Container();
                c.addLeft(Integer.valueOf(7));
                assertThat(c.getLeft(), is(Integer.valueOf(7)));
        }

        @Test
        void addLeftWhenSetAccumulates() {
                Container c = new Container();
                c.setLeft(Integer.valueOf(3));
                c.addLeft(Integer.valueOf(4));
                assertThat(c.getLeft(), is(Integer.valueOf(7)));
        }

        @Test
        void topRoundTrip() {
                Container c = new Container();
                c.setTop(Integer.valueOf(10));
                assertThat(c.getTop(), is(Integer.valueOf(10)));
        }

        @Test
        void containerTypeRoundTrip() {
                Container c = new Container();
                c.setContainerType(Container.ContainerType.tab);
                assertThat(c.getContainerType(), is(Container.ContainerType.tab));
        }

        @Test
        void getParentReturnsNullInitially() {
                Container c = new Container();
                assertThat(c.getParent(), nullValue());
        }

        @Test
        void setHorizontalRoundTrip() {
                Container c = new Container();
                c.setHorizontal(Boolean.TRUE);
                assertThat(c.getHorizontal(), is(Boolean.TRUE));
        }

        @Test
        void setElementsRoundTrip() {
                Container c = new Container();
                java.util.List<ReportElement> elems = new java.util.ArrayList<>();
                c.setElements(elems);
                assertThat(c.getElements(), org.hamcrest.CoreMatchers.sameInstance(elems));
        }

        @Test
        void setContainersRoundTrip() {
                Container c = new Container();
                java.util.List<Container> containers = new java.util.ArrayList<>();
                c.setContainers(containers);
                assertThat(c.getContainers(), org.hamcrest.CoreMatchers.sameInstance(containers));
        }
}
