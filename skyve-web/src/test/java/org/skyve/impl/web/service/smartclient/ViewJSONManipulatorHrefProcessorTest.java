package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.reference.ActionReference;
import org.skyve.impl.metadata.view.reference.ContentReference;
import org.skyve.impl.metadata.view.reference.EditViewReference;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.reference.ImplicitActionReference;
import org.skyve.impl.metadata.view.reference.ReportReference;
import org.skyve.impl.metadata.view.reference.ResourceReference;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.report.ReportFormat;

@SuppressWarnings("static-method")
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
	void actionReferenceSendsViewActionClassNameAndClientValidation() throws Exception {
		ViewJSONManipulator manipulator = newManipulator("edit", 5, 9);
		setCurrentBindings(manipulator, "row");
		ActionImpl action = new ActionImpl();
		action.setName("doThing");
		action.setResourceName("DoThingAction");
		view(manipulator).putAction(action);

		ActionReference ref = new ActionReference();
		ref.setActionName("doThing");
		invokeHrefProcessor(manipulator, "processActionReference", ActionReference.class, ref);

		assertEquals("javascript:testModule_TestDoc_edit_5._view.doAction('DoThingAction',true,'row'," +
						"'{bizModule}','{bizDocument}','{bizId}')",
						htmlGuts(manipulator));
	}

	@Test
	void actionReferenceWithConfirmationAsksFirstWithEscapedBraces() throws Exception {
		ViewJSONManipulator manipulator = newManipulator("edit", 5, 9);
		setCurrentBindings(manipulator, "row");
		putFormAction(manipulator, "Isn't it?", null);

		ActionReference ref = new ActionReference();
		ref.setActionName("doThing");
		invokeHrefProcessor(manipulator, "processActionReference", ActionReference.class, ref);

		// HTML escaped as a button's confirm is, then JavaScript escaped for the href attribute
		assertEquals("javascript:isc.ask('Isn\\x26#39;t it?',function(b)\\{if(b)\\{" +
						"testModule_TestDoc_edit_5._view.doAction('DoThingAction',true,'row'," +
						"'{bizModule}','{bizDocument}','{bizId}')\\}\\},\\{title:isc.i18nMessages.editView_Confirm\\})",
						htmlGuts(manipulator));
	}

	@Test
	void confirmationQuotesAndAmpersandsCannotEndOrChangeTheHrefAttribute() throws Exception {
		ViewJSONManipulator manipulator = newManipulator("edit", 5, 9);
		setCurrentBindings(manipulator, "row");
		putFormAction(manipulator, "Say \"hi\" & 'bye'", Boolean.FALSE);

		ActionReference ref = new ActionReference();
		ref.setActionName("doThing");
		invokeHrefProcessor(manipulator, "processActionReference", ActionReference.class, ref);

		String guts = htmlGuts(manipulator);
		assertTrue(guts.startsWith("javascript:isc.ask('Say \\x22hi\\x22 \\x26 \\x27bye\\x27',"), guts);
		// The href is double-quoted and HTML decodes it, so neither may appear raw
		assertFalse(guts.contains("\""), guts);
		assertFalse(guts.contains("&"), guts);
	}

	@Test
	void confirmationBracesAreLiteralTextNotBindings() throws Exception {
		ViewJSONManipulator manipulator = newManipulator("edit", 5, 9);
		setCurrentBindings(manipulator, "row");
		// An unbalanced brace would make formatting the link throw if it were read as a binding
		putFormAction(manipulator, "Keep {name} and \\{ { ?", Boolean.FALSE);

		ActionReference ref = new ActionReference();
		ref.setActionName("doThing");
		invokeHrefProcessor(manipulator, "processActionReference", ActionReference.class, ref);

		// Format the link as constructJSONObjectFromBinding() does, with the row placeholders already resolved
		String guts = htmlGuts(manipulator).replace("'{bizModule}','{bizDocument}','{bizId}'", "'m','d','i'");
		assertEquals("javascript:isc.ask('Keep {name} and \\\\{ { ?',function(b){if(b){" +
						"testModule_TestDoc_edit_5._view.doAction('DoThingAction',true,'row','m','d','i')}}," +
						"{title:isc.i18nMessages.editView_Confirm})",
						BindUtil.formatMessage(guts));
	}

	@Test
	void confirmationMarkupIsEscapedUnlessEscapeConfirmIsFalse() throws Exception {
		ViewJSONManipulator escaped = newManipulator("edit", 5, 9);
		setCurrentBindings(escaped, "row");
		putFormAction(escaped, "<b>Sure</b>?", null);
		ViewJSONManipulator unescaped = newManipulator("edit", 5, 9);
		setCurrentBindings(unescaped, "row");
		putFormAction(unescaped, "<b>Sure</b>?", Boolean.FALSE);

		ActionReference ref = new ActionReference();
		ref.setActionName("doThing");
		invokeHrefProcessor(escaped, "processActionReference", ActionReference.class, ref);
		invokeHrefProcessor(unescaped, "processActionReference", ActionReference.class, ref);

		assertTrue(htmlGuts(escaped).startsWith("javascript:isc.ask('\\x26lt;b\\x26gt;Sure\\x26lt;/b\\x26gt;?',"),
					htmlGuts(escaped));
		assertTrue(htmlGuts(unescaped).startsWith("javascript:isc.ask('<b>Sure</b>?',"), htmlGuts(unescaped));
	}

	@Test
	void actionReferenceInDataWidgetUsesRowDocumentEditViewAction() throws Exception {
		ViewJSONManipulator manipulator = newManipulator("edit", 5, 9);
		setCurrentBindings(manipulator, "row");
		ViewImpl rowEditView = new ViewImpl();
		rowEditView.putAction(rowAction());
		Document rowDocument = mock(Document.class);
		when(rowDocument.getView(eq("external"), any(), eq("edit"))).thenReturn(rowEditView);
		setDataWidgetDocument(manipulator, rowDocument);

		ActionReference ref = new ActionReference();
		ref.setActionName("doThing");
		invokeHrefProcessor(manipulator, "processActionReference", ActionReference.class, ref);

		assertTrue(htmlGuts(manipulator).startsWith("javascript:testModule_TestDoc_edit_5._view.doAction('DoRowThingAction',false,'row'"));
	}

	@Test
	void actionReferenceInDataWidgetFallsBackToRowDocumentCreateViewAction() throws Exception {
		ViewJSONManipulator manipulator = newManipulator("edit", 5, 9);
		setCurrentBindings(manipulator, "row");
		ViewImpl rowCreateView = new ViewImpl();
		rowCreateView.putAction(rowAction());
		Document rowDocument = mock(Document.class);
		when(rowDocument.getView(eq("external"), any(), eq("edit"))).thenReturn(new ViewImpl());
		when(rowDocument.getView(eq("external"), any(), eq("create"))).thenReturn(rowCreateView);
		setDataWidgetDocument(manipulator, rowDocument);

		ActionReference ref = new ActionReference();
		ref.setActionName("doThing");
		invokeHrefProcessor(manipulator, "processActionReference", ActionReference.class, ref);

		assertTrue(htmlGuts(manipulator).startsWith("javascript:testModule_TestDoc_edit_5._view.doAction('DoRowThingAction',false,'row'"));
	}

	@Test
	void actionReferenceInDataWidgetSkipsAMissingRowDocumentView() throws Exception {
		ViewJSONManipulator manipulator = newManipulator("edit", 5, 9);
		setCurrentBindings(manipulator, "row");
		ViewImpl rowCreateView = new ViewImpl();
		rowCreateView.putAction(rowAction());
		Document rowDocument = mock(Document.class);
		when(rowDocument.getView(eq("external"), any(), eq("edit"))).thenThrow(new MetaDataException("No edit view"));
		when(rowDocument.getView(eq("external"), any(), eq("create"))).thenReturn(rowCreateView);
		setDataWidgetDocument(manipulator, rowDocument);

		ActionReference ref = new ActionReference();
		ref.setActionName("doThing");
		invokeHrefProcessor(manipulator, "processActionReference", ActionReference.class, ref);

		assertTrue(htmlGuts(manipulator).startsWith("javascript:testModule_TestDoc_edit_5._view.doAction('DoRowThingAction',false,'row'"));
	}

	@Test
	void actionReferenceInDataWidgetWithNoRowActionSendsTheReferencedName() throws Exception {
		ViewJSONManipulator manipulator = newManipulator("edit", 5, 9);
		setCurrentBindings(manipulator, "row");
		// The form view's action of the same name must not be used for a row link
		putFormAction(manipulator, "Sure?", null);
		Document rowDocument = mock(Document.class);
		when(rowDocument.getView(eq("external"), any(), any())).thenThrow(new MetaDataException("No view"));
		setDataWidgetDocument(manipulator, rowDocument);

		ActionReference ref = new ActionReference();
		ref.setActionName("doThing");
		invokeHrefProcessor(manipulator, "processActionReference", ActionReference.class, ref);

		assertEquals("javascript:testModule_TestDoc_edit_5._view.doAction('doThing',false,'row'," +
						"'{bizModule}','{bizDocument}','{bizId}')",
						htmlGuts(manipulator));
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

		setHtmlGuts(manipulator, new StringBuilder());
		EditViewReference editViewWithoutBinding = new EditViewReference();
		editViewWithoutBinding.setModuleName("test");
		editViewWithoutBinding.setDocumentName("AllAttributesPersistent");
		invokeHrefProcessor(manipulator, "processEditViewReference", EditViewReference.class, editViewWithoutBinding);
		assertTrue(htmlGuts(manipulator).contains("m=test"));
		assertTrue(htmlGuts(manipulator).contains("d=AllAttributesPersistent"));
		assertFalse(htmlGuts(manipulator).contains("{row.bizId}"));
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

	private static ViewImpl view(ViewJSONManipulator manipulator) throws Exception {
		Field field = org.skyve.impl.metadata.view.ViewVisitor.class.getDeclaredField("view");
		field.setAccessible(true);
		return (ViewImpl) field.get(manipulator);
	}

	/**
	 * Declares a form view action named doThing, of class DoThingAction, with a confirmation.
	 */
	private static void putFormAction(ViewJSONManipulator manipulator, String confirmationText, Boolean escapeConfirm)
	throws Exception {
		ActionImpl action = new ActionImpl();
		action.setName("doThing");
		action.setResourceName("DoThingAction");
		action.setConfirmationText(confirmationText);
		action.setEscapeConfirm(escapeConfirm);
		view(manipulator).putAction(action);
	}

	/**
	 * Returns a row view action named doThing, of class DoRowThingAction, without client validation.
	 */
	private static ActionImpl rowAction() {
		ActionImpl result = new ActionImpl();
		result.setName("doThing");
		result.setResourceName("DoRowThingAction");
		result.setClientValidation(Boolean.FALSE);
		return result;
	}

	private static void setDataWidgetDocument(ViewJSONManipulator manipulator, Document value) throws Exception {
		Field field = ViewJSONManipulator.class.getDeclaredField("dataWidgetDocument");
		field.setAccessible(true);
		field.set(manipulator, value);
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
