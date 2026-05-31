package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.reference.ActionReference;
import org.skyve.impl.metadata.view.reference.ContentReference;
import org.skyve.impl.metadata.view.reference.EditViewReference;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.reference.ImplicitActionReference;
import org.skyve.impl.metadata.view.reference.ReportReference;
import org.skyve.impl.metadata.view.reference.ResourceReference;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.user.User;
import org.skyve.report.ReportFormat;

class ViewJSONManipulatorHrefProcessorTest {
	@Test
	void actionReferenceUsesGeneratedWidgetIdAndBindingPrefix() throws Exception {
		ViewJSONManipulator manipulator = newManipulator("edit", 5, 9);
		setCurrentBindings(manipulator, "row");

		ActionReference ref = new ActionReference();
		ref.setActionName("doThing");
		invokeHrefProcessor(manipulator, "processActionReference", ActionReference.class, ref);

		assertTrue(htmlGuts(manipulator).contains("javascript:testModule_TestDoc_edit_5._view.doAction('doThing',false,'row'"));
	}

	@Test
	void implicitRemoveRendersOnlyWhenProcessingDataWidget() throws Exception {
		ViewJSONManipulator manipulator = newManipulator("create", 7, 3);

		ImplicitActionReference removeRef = new ImplicitActionReference();
		removeRef.setImplicitActionName(ImplicitActionName.Remove);

		setProcessingDataWidget(manipulator, false);
		invokeHrefProcessor(manipulator, "processImplicitActionReference", ImplicitActionReference.class, removeRef);
		assertEquals("", htmlGuts(manipulator));

		setHtmlGuts(manipulator, new StringBuilder());
		setProcessingDataWidget(manipulator, true);
		invokeHrefProcessor(manipulator, "processImplicitActionReference", ImplicitActionReference.class, removeRef);
		assertTrue(htmlGuts(manipulator).contains("javascript:testModule_TestDoc_create_3.remove('{bizId}')"));

		setHtmlGuts(manipulator, new StringBuilder());
		ImplicitActionReference nonRemoveRef = new ImplicitActionReference();
		nonRemoveRef.setImplicitActionName(ImplicitActionName.Delete);
		invokeHrefProcessor(manipulator, "processImplicitActionReference", ImplicitActionReference.class, nonRemoveRef);
		assertEquals("", htmlGuts(manipulator));
	}

	@Test
	void reportReferenceIncludesBindingAndStaticParameters() throws Exception {
		ViewJSONManipulator manipulator = newManipulator("edit", 1, 1);

		ReportReference ref = new ReportReference();
		ref.setFormat(ReportFormat.pdf);
		ref.setReportName("Sales");
		ref.setModuleName("testModule");
		ref.setDocumentName("TestDoc");

		ParameterImpl bindingParam = new ParameterImpl();
		bindingParam.setName("p1");
		bindingParam.setValueBinding("row.total");
		ref.getParameters().add(bindingParam);

		ParameterImpl valueParam = new ParameterImpl();
		valueParam.setName("p2");
		valueParam.setValue("fixed");
		ref.getParameters().add(valueParam);

		invokeHrefProcessor(manipulator, "processReportReference", ReportReference.class, ref);
		String html = htmlGuts(manipulator);
		assertTrue(html.contains("report.rpt?_format=pdf&_id={bizId}&_n=Sales&_doc=testModule.TestDoc"));
		assertTrue(html.contains("&p1={row.total}"));
		assertTrue(html.contains("&p2=fixed"));
	}

	@Test
	void contentResourceExternalAndEditReferencesAppendExpectedUrls() throws Exception {
		ViewJSONManipulator manipulator = newManipulator("edit", 1, 1);

		ContentReference contentRef = new ContentReference();
		contentRef.setBinding("contentBinding");
		invokeHrefProcessor(manipulator, "processContentReference", ContentReference.class, contentRef);
		assertTrue(htmlGuts(manipulator).contains("content?_n={contentBinding}&_doc={bizModule}.{bizDocument}&_b=contentBinding"));

		setHtmlGuts(manipulator, new StringBuilder());
		ResourceReference resourceRef = new ResourceReference();
		resourceRef.setRelativeFile("images/test.png");
		invokeHrefProcessor(manipulator, "processResourceReference", ResourceReference.class, resourceRef);
		assertTrue(htmlGuts(manipulator).contains("resources?_doc={bizModule}.{bizDocument}&_n=images/test.png"));

		setHtmlGuts(manipulator, new StringBuilder());
		ExternalReference externalRef = new ExternalReference();
		externalRef.setHref("https://example.com");
		invokeHrefProcessor(manipulator, "processExternalReference", ExternalReference.class, externalRef);
		assertEquals("https://example.com", htmlGuts(manipulator));

		setHtmlGuts(manipulator, new StringBuilder());
		EditViewReference editViewReference = new EditViewReference();
		editViewReference.setModuleName("test");
		editViewReference.setDocumentName("AllAttributesPersistent");
		editViewReference.setBinding("row.bizId");
		invokeHrefProcessor(manipulator, "processEditViewReference", EditViewReference.class, editViewReference);
		assertTrue(htmlGuts(manipulator).contains("{row.bizId}"));
	}

	private static ViewJSONManipulator newManipulator(String viewName, int editIdCounter, int createIdCounter) {
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(new CustomerImpl());

		ModuleImpl module = new ModuleImpl();
		module.setName("testModule");

		DocumentImpl document = new DocumentImpl();
		document.setName("TestDoc");
		document.setPersistent(new Persistent());

		ViewImpl view = new ViewImpl();
		view.setName(viewName);

		return new ViewJSONManipulator(user,
				module,
				document,
				view,
				"external",
				null,
				editIdCounter,
				createIdCounter,
				false);
	}

	private static void invokeHrefProcessor(ViewJSONManipulator manipulator, String methodName, Class<?> argType, Object arg)
	throws Exception {
		Object hrefProcessor = hrefProcessor(manipulator);
		Method method = hrefProcessor.getClass().getDeclaredMethod(methodName, argType);
		method.setAccessible(true);
		method.invoke(hrefProcessor, arg);
	}

	private static Object hrefProcessor(ViewJSONManipulator manipulator) throws Exception {
		Field field = ViewJSONManipulator.class.getDeclaredField("hrefProcessor");
		field.setAccessible(true);
		return field.get(manipulator);
	}

	private static void setCurrentBindings(ViewJSONManipulator manipulator, String prefix) throws Exception {
		Field documentField = org.skyve.impl.metadata.view.ViewVisitor.class.getDeclaredField("document");
		documentField.setAccessible(true);
		DocumentImpl document = (DocumentImpl) documentField.get(manipulator);

		ViewBindings root = new ViewBindings(document);
		ViewBindings child = root.putOrGetChild(prefix, document);

		Field currentBindings = ViewJSONManipulator.class.getDeclaredField("currentBindings");
		currentBindings.setAccessible(true);
		currentBindings.set(manipulator, child);
	}

	private static void setProcessingDataWidget(ViewJSONManipulator manipulator, boolean value) throws Exception {
		Field field = ViewJSONManipulator.class.getDeclaredField("processingDataWidget");
		field.setAccessible(true);
		field.setBoolean(manipulator, value);
	}

	private static String htmlGuts(ViewJSONManipulator manipulator) throws Exception {
		Field field = ViewJSONManipulator.class.getDeclaredField("htmlGuts");
		field.setAccessible(true);
		StringBuilder sb = (StringBuilder) field.get(manipulator);
		return sb.toString();
	}

	private static void setHtmlGuts(ViewJSONManipulator manipulator, StringBuilder value) throws Exception {
		Field field = ViewJSONManipulator.class.getDeclaredField("htmlGuts");
		field.setAccessible(true);
		field.set(manipulator, value);
	}
}
