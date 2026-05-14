package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.ViewMetaData;

public class FluentViewTest {

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
}
