package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Sidebar;

class FluentViewTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesView() {
		FluentView v = new FluentView();
		assertNotNull(v.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void viewMetaDataConstructorUsesProvided() {
		ViewMetaData vmd = new ViewMetaData();
		FluentView v = new FluentView(vmd);
		assertSame(vmd, v.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void nameSetterAndGet() {
		FluentView v = new FluentView();
		FluentView result = v.name("editView");
		assertSame(v, result);
		assertEquals("editView", v.get().getName());
	}

	@Test
	@SuppressWarnings("static-method")
	void titleSetterAndGet() {
		FluentView v = new FluentView();
		FluentView result = v.title("My Title");
		assertSame(v, result);
		assertEquals("My Title", v.get().getTitle());
	}

	@Test
	@SuppressWarnings("static-method")
	void iconStyleClassSetterAndGet() {
		FluentView v = new FluentView();
		FluentView result = v.iconStyleClass("fa fa-home");
		assertSame(v, result);
		assertEquals("fa fa-home", v.get().getIconStyleClass());
	}

	@Test
	@SuppressWarnings("static-method")
	void icon32x32RelativeFileNameSetterAndGet() {
		FluentView v = new FluentView();
		FluentView result = v.icon32x32RelativeFileName("icon.png");
		assertSame(v, result);
		assertEquals("icon.png", v.get().getIcon32x32RelativeFileName());
	}

	@Test
	@SuppressWarnings("static-method")
	void documentationSetterAndGet() {
		FluentView v = new FluentView();
		FluentView result = v.documentation("Some docs");
		assertSame(v, result);
		assertEquals("Some docs", v.get().getDocumentation());
	}

	@Test
	@SuppressWarnings("static-method")
	void addAndFindDocumentAggregateAccess() {
		FluentView v = new FluentView();
		FluentViewDocumentAggregateAccess access = new FluentViewDocumentAggregateAccess().documentName("TestDoc");
		FluentView result = v.addDocumentAggregateAccess(access);
		assertSame(v, result);
		assertNotNull(v.findDocumentAggregateAccess("TestDoc"));
	}

	@Test
	@SuppressWarnings("static-method")
	void findDocumentAggregateAccessReturnsNullWhenNotFound() {
		FluentView v = new FluentView();
		assertNull(v.findDocumentAggregateAccess("Missing"));
	}

	@Test
	@SuppressWarnings("static-method")
	void removeDocumentAggregateAccess() {
		FluentView v = new FluentView();
		v.addDocumentAggregateAccess(new FluentViewDocumentAggregateAccess().documentName("TestDoc"));
		FluentView result = v.removeDocumentAggregateAccess("TestDoc");
		assertSame(v, result);
		assertNull(v.findDocumentAggregateAccess("TestDoc"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addAndFindQueryAggregateAccess() {
		FluentView v = new FluentView();
		FluentViewQueryAggregateAccess access = new FluentViewQueryAggregateAccess().queryName("TestQuery");
		FluentView result = v.addQueryAggregateAccess(access);
		assertSame(v, result);
		assertNotNull(v.findQueryAggregateAccess("TestQuery"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addAndFindReportAccess() {
		FluentView v = new FluentView();
		FluentViewReportAccess access = new FluentViewReportAccess().moduleName("TestMod").documentName("TestDoc").reportName("TestReport");
		FluentView result = v.addReportAccess(access);
		assertSame(v, result);
		assertNotNull(v.findReportAccess("TestMod", "TestDoc", "TestReport"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addAndFindSingularAccess() {
		FluentView v = new FluentView();
		FluentViewSingularAccess access = new FluentViewSingularAccess().documentName("TestDoc");
		FluentView result = v.addSingularAccess(access);
		assertSame(v, result);
		assertNotNull(v.findSingularAccess("TestDoc"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addAndFindModelAggregateAccess() {
		FluentView v = new FluentView();
		FluentViewModelAggregateAccess access = new FluentViewModelAggregateAccess().modelName("TestModel");
		FluentView result = v.addModelAggregateAccess(access);
		assertSame(v, result);
		assertNotNull(v.findModelAggregateAccess("TestModel"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addAndFindPreviousCompleteAccess() {
		FluentView v = new FluentView();
		FluentViewPreviousCompleteAccess access = new FluentViewPreviousCompleteAccess().binding("someBinding");
		FluentView result = v.addPreviousCompleteAccess(access);
		assertSame(v, result);
		assertNotNull(v.findPreviousCompleteAccess("someBinding"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addAndFindContentAccess() {
		FluentView v = new FluentView();
		FluentViewContentAccess access = new FluentViewContentAccess().binding("content");
		FluentView result = v.addContentAccess(access);
		assertSame(v, result);
		assertNotNull(v.findContentAccess("content"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addAndFindDynamicImageAccess() {
		FluentView v = new FluentView();
		FluentViewDynamicImageAccess access = new FluentViewDynamicImageAccess().imageName("myImage");
		FluentView result = v.addDynamicImageAccess(access);
		assertSame(v, result);
		assertNotNull(v.findDynamicImageAccess("myImage"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addAndFindParameter() {
		FluentView v = new FluentView();
		FluentViewParameter param = new FluentViewParameter().fromBinding("fromField");
		FluentView result = v.addParameter(param);
		assertSame(v, result);
		assertNotNull(v.findParameter("fromField"));
	}

        @Test
        @SuppressWarnings("static-method")
        void helpRelativeFileNameSetterAndGet() {
                FluentView v = new FluentView();
                assertSame(v, v.helpRelativeFileName("help.html"));
                assertEquals("help.html", v.get().getHelpRelativeFileName());
        }

        @Test
        @SuppressWarnings("static-method")
        void helpURLSetterAndGet() {
                FluentView v = new FluentView();
                assertSame(v, v.helpURL("https://example.com"));
                assertEquals("https://example.com", v.get().getHelpURL());
        }

        @Test
        @SuppressWarnings("static-method")
        void refreshTimeInSecondsSetterAndGet() {
                FluentView v = new FluentView();
                assertSame(v, v.refreshTimeInSeconds(30));
                assertEquals(Integer.valueOf(30), v.get().getRefreshTimeInSeconds());
        }

        @Test
        @SuppressWarnings("static-method")
        void refreshConditionNameSetterAndGet() {
                FluentView v = new FluentView();
                assertSame(v, v.refreshConditionName("someCondition"));
                assertEquals("someCondition", v.get().getRefreshConditionName());
        }

        @Test
        @SuppressWarnings("static-method")
        void refreshActionNameSetterAndGet() {
                FluentView v = new FluentView();
                assertSame(v, v.refreshActionName("SomeAction"));
                assertEquals("SomeAction", v.get().getRefreshActionName());
        }

        @Test
        @SuppressWarnings("static-method")
        void removeParameterRemovesIt() {
                FluentView v = new FluentView();
                v.addParameter(new FluentViewParameter().fromBinding("f1"));
                v.removeParameter("f1");
                assertNull(v.findParameter("f1"));
        }

        @Test
        @SuppressWarnings("static-method")
        void clearParametersClearsAll() {
                FluentView v = new FluentView();
                v.addParameter(new FluentViewParameter().fromBinding("f1"));
                v.addParameter(new FluentViewParameter().fromBinding("f2"));
                v.clearParameters();
                assertNull(v.findParameter("f1"));
        }

        @Test
        @SuppressWarnings("static-method")
        void generateAccessesSetsFlag() {
                FluentView v = new FluentView();
                assertSame(v, v.generateAccesses(true));
        }

        @Test
        @SuppressWarnings("static-method")
        void clearAccessesClearsAll() {
                FluentView v = new FluentView();
                v.addDocumentAggregateAccess(new FluentViewDocumentAggregateAccess().documentName("Doc"));
                v.clearAccesses();
                assertNull(v.findDocumentAggregateAccess("Doc"));
        }

        @Test
        @SuppressWarnings("static-method")
        void removeAccessesRemovesThem() {
                FluentView v = new FluentView();
                v.addDocumentAggregateAccess(new FluentViewDocumentAggregateAccess().documentName("Doc"));
                v.removeAccesses();
                assertNull(v.findDocumentAggregateAccess("Doc"));
        }

        @Test
        @SuppressWarnings("static-method")
        void removeQueryAggregateAccess() {
                FluentView v = new FluentView();
                v.addQueryAggregateAccess(new FluentViewQueryAggregateAccess().queryName("Q"));
                v.removeQueryAggregateAccess("Q");
                assertNull(v.findQueryAggregateAccess("Q"));
        }

        @Test
        @SuppressWarnings("static-method")
        void removeSingularAccess() {
                FluentView v = new FluentView();
                v.addSingularAccess(new FluentViewSingularAccess().documentName("Doc"));
                v.removeSingularAccess("Doc");
                assertNull(v.findSingularAccess("Doc"));
        }

        @Test
        @SuppressWarnings("static-method")
        void removeReportAccess() {
                FluentView v = new FluentView();
                v.addReportAccess(new FluentViewReportAccess().moduleName("m").documentName("d").reportName("r"));
                v.removeReportAccess("m", "d", "r");
                assertNull(v.findReportAccess("m", "d", "r"));
        }

        @Test
        @SuppressWarnings("static-method")
        void removeContentAccess() {
                FluentView v = new FluentView();
                v.addContentAccess(new FluentViewContentAccess().binding("b"));
                v.removeContentAccess("b");
                assertNull(v.findContentAccess("b"));
        }

        @Test
        @SuppressWarnings("static-method")
        void removeDynamicImageAccess() {
                FluentView v = new FluentView();
                v.addDynamicImageAccess(new FluentViewDynamicImageAccess().imageName("img"));
                v.removeDynamicImageAccess("img");
                assertNull(v.findDynamicImageAccess("img"));
        }

        @Test
        @SuppressWarnings("static-method")
        void removePreviousCompleteAccess() {
                FluentView v = new FluentView();
                v.addPreviousCompleteAccess(new FluentViewPreviousCompleteAccess().binding("b"));
                v.removePreviousCompleteAccess("b");
                assertNull(v.findPreviousCompleteAccess("b"));
        }

        @Test
        @SuppressWarnings("static-method")
        void removeModelAggregateAccess() {
                FluentView v = new FluentView();
                v.addModelAggregateAccess(new FluentViewModelAggregateAccess().modelName("MyModel"));
                v.removeModelAggregateAccess("MyModel");
                assertNull(v.findModelAggregateAccess("MyModel"));
        }

        @Test
        @SuppressWarnings("static-method")
        void sidebarSetsValue() {
                FluentView v = new FluentView();
                FluentSidebar sidebar = new FluentSidebar();
                v.sidebar(sidebar);
                assertNotNull(v.get().getSidebar());
        }

        @Test
        @SuppressWarnings("static-method")
        void actionsSetsValue() {
                FluentView v = new FluentView();
                FluentActions actions = new FluentActions().widgetId("w1");
                v.actions(actions);
                assertNotNull(v.get().getActions());
        }

        @Test
        @SuppressWarnings("static-method")
        void removeAccesses() {
                FluentView v = new FluentView();
                v.addSingularAccess(new FluentViewSingularAccess().documentName("Doc"));
                v.removeAccesses();
                assertNull(v.get().getAccesses());
        }

	@Test
	@SuppressWarnings("static-method")
	void fromCopiesNameTitleAndHelpFields() {
		ViewImpl impl = new ViewImpl();
		impl.setName("editView");
		impl.setTitle("Edit Contact");
		impl.setHelpURL("https://example.com/help");
		impl.setHelpRelativeFileName("help.html");

		FluentView v = new FluentView().from((org.skyve.metadata.view.View) impl);
		assertEquals("editView", v.get().getName());
		assertEquals("Edit Contact", v.get().getTitle());
		assertEquals("https://example.com/help", v.get().getHelpURL());
		assertEquals("help.html", v.get().getHelpRelativeFileName());
	}

	@Test
	@SuppressWarnings("static-method")
	void fromWithRefreshTimeCopiesValue() {
		ViewImpl impl = new ViewImpl();
		impl.setName("v");
		impl.setRefreshTimeInSeconds(Integer.valueOf(30));

		FluentView v = new FluentView().from((org.skyve.metadata.view.View) impl);
		assertEquals(Integer.valueOf(30), v.get().getRefreshTimeInSeconds());
	}

	@Test
	@SuppressWarnings("static-method")
	void fromWithNullRefreshTimeSkipsIt() {
		ViewImpl impl = new ViewImpl();
		impl.setName("v");
		// refreshTimeInSeconds is null by default
		FluentView v = new FluentView().from((org.skyve.metadata.view.View) impl);
		assertNull(v.get().getRefreshTimeInSeconds());
	}

	@Test
	@SuppressWarnings("static-method")
	void fromWithSidebarCopiesSidebar() {
		ViewImpl impl = new ViewImpl();
		impl.setName("v");
		impl.setSidebar(new Sidebar());
		FluentView v = new FluentView().from((org.skyve.metadata.view.View) impl);
		assertNotNull(v.get().getSidebar());
	}

	@Test
	@SuppressWarnings("static-method")
	void findModelAggregateAccessReturnsNullWhenNoAccesses() {
		assertNull(new FluentView().findModelAggregateAccess("model"));
	}

	@Test
	@SuppressWarnings("static-method")
	void findPreviousCompleteAccessReturnsNullWhenNoAccesses() {
		assertNull(new FluentView().findPreviousCompleteAccess("binding"));
	}

	@Test
	@SuppressWarnings("static-method")
	void findQueryAggregateAccessReturnsNullWhenNoAccesses() {
		assertNull(new FluentView().findQueryAggregateAccess("query"));
	}

	@Test
	@SuppressWarnings("static-method")
	void findSingularAccessReturnsNullWhenNoAccesses() {
		assertNull(new FluentView().findSingularAccess("Doc"));
	}

	@Test
	@SuppressWarnings("static-method")
	void findReportAccessReturnsNullWhenNoAccesses() {
		assertNull(new FluentView().findReportAccess("mod", "doc", "report"));
	}

	@Test
	@SuppressWarnings("static-method")
	void findDynamicImageAccessReturnsNullWhenNoAccesses() {
		assertNull(new FluentView().findDynamicImageAccess("image"));
	}

	@Test
	@SuppressWarnings("static-method")
	void findContentAccessReturnsNullWhenNoAccesses() {
		assertNull(new FluentView().findContentAccess("binding"));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromWithNonEmptyParametersCopiesParametersViaLambda() {
		// Exercises: view.getParameters().forEach(p -> addParameter(new FluentViewParameter().from(p)))
		ViewImpl impl = new ViewImpl();
		impl.getParameters().add(new FluentViewParameter().fromBinding("field1").get());
		FluentView v = new FluentView().from((org.skyve.metadata.view.View) impl);
		assertNotNull(v.findParameter("field1"));
	}
}
