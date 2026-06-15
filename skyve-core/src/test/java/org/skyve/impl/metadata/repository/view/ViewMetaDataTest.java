package org.skyve.impl.metadata.repository.view;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessesMetaData;
import org.skyve.impl.metadata.repository.view.actions.BizExportAction;
import org.skyve.impl.metadata.repository.view.actions.CustomAction;
import org.skyve.impl.metadata.repository.view.actions.ReportAction;
import org.skyve.impl.metadata.repository.view.actions.SaveAction;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.view.View.ViewParameter;

@SuppressWarnings("static-method")
class ViewMetaDataTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new ViewMetaData());
	}

	@Test
	void nameRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		assertEquals("edit", v.getName());
	}

	@Test
	void nameNullByDefault() {
		assertNull(new ViewMetaData().getName());
	}

	@Test
	void titleRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setTitle("My View");
		assertEquals("My View", v.getTitle());
	}

	@Test
	void titleNullByDefault() {
		assertNull(new ViewMetaData().getTitle());
	}

	@Test
	void icon32x32RelativeFileNameRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setIcon32x32RelativeFileName("icon.png");
		assertEquals("icon.png", v.getIcon32x32RelativeFileName());
	}

	@Test
	void iconStyleClassRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setIconStyleClass("fa-edit");
		assertEquals("fa-edit", v.getIconStyleClass());
	}

	@Test
	void helpRelativeFileNameRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setHelpRelativeFileName("help.html");
		assertEquals("help.html", v.getHelpRelativeFileName());
	}

	@Test
	void helpURLRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setHelpURL("https://example.com/help");
		assertEquals("https://example.com/help", v.getHelpURL());
	}

	@Test
	void refreshTimeInSecondsRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setRefreshTimeInSeconds(Integer.valueOf(30));
		assertEquals(Integer.valueOf(30), v.getRefreshTimeInSeconds());
	}

	@Test
	void refreshTimeInSecondsNullByDefault() {
		assertNull(new ViewMetaData().getRefreshTimeInSeconds());
	}

	@Test
	void refreshConditionNameRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setRefreshConditionName("canRefresh");
		assertEquals("canRefresh", v.getRefreshConditionName());
	}

	@Test
	void refreshActionNameRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setRefreshActionName("MyAction");
		assertEquals("MyAction", v.getRefreshActionName());
	}

	@Test
	void parametersListNonNull() {
		assertNotNull(new ViewMetaData().getParameters());
	}

	@Test
	void documentationRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setDocumentation("Some docs");
		assertEquals("Some docs", v.getDocumentation());
	}

	@Test
	void documentationNullByDefault() {
		assertNull(new ViewMetaData().getDocumentation());
	}

	@Test
	void lastModifiedMillisDefaultIsMaxValue() {
		assertEquals(Long.MAX_VALUE, new ViewMetaData().getLastModifiedMillis());
	}

	@Test
	void lastModifiedMillisRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setLastModifiedMillis(1234567890L);
		assertEquals(1234567890L, v.getLastModifiedMillis());
	}

	@Test
	void propertiesMapNonNull() {
		assertNotNull(new ViewMetaData().getProperties());
	}

	@Test
	void sidebarNullByDefault() {
		assertNull(new ViewMetaData().getSidebar());
	}

	@Test
	void actionsNullByDefault() {
		assertNull(new ViewMetaData().getActions());
	}

	@Test
	void accessesNullByDefault() {
		assertNull(new ViewMetaData().getAccesses());
	}

	@Test
	void sidebarRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		Sidebar sidebar = new Sidebar();
		v.setSidebar(sidebar);
		assertSame(sidebar, v.getSidebar());
	}

	@Test
	void actionsRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		Actions actions = new Actions();
		v.setActions(actions);
		assertSame(actions, v.getActions());
	}

	@Test
	void accessesRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		ViewUserAccessesMetaData accesses = new ViewUserAccessesMetaData();
		v.setAccesses(accesses);
		assertSame(accesses, v.getAccesses());
	}

	@Test
	void convertThrowsWhenTitleIsNull() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		// no title set → MetaDataException
		assertThrows(MetaDataException.class, () -> v.convert("TestModule.TestDoc.edit.xml"));
	}

	@Test
	void convertThrowsWhenNameIsNull() {
		ViewMetaData v = new ViewMetaData();
		v.setTitle("My View");
		// no name set → MetaDataException
		assertThrows(MetaDataException.class, () -> v.convert("TestModule.TestDoc.edit.xml"));
	}

	@Test
	void convertSucceedsWithMinimalConfig() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		ViewImpl result = v.convert("TestModule.TestDoc.edit.xml");
		assertNotNull(result);
		assertEquals("edit", result.getName());
		assertEquals("My View", result.getTitle());
	}

	@Test
	void convertSetsOptionalFieldsWhenProvided() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		v.setIconStyleClass("fa-edit");
		v.setHelpURL("http://example.com/help");
		v.setLastModifiedMillis(999L);
		ViewImpl result = v.convert("TestModule.TestDoc.edit.xml");
		assertEquals("fa-edit", result.getIconStyleClass());
		assertEquals("http://example.com/help", result.getHelpURL());
		assertEquals(999L, result.getLastModifiedMillis());
	}

	@Test
	void convertThrowsWhenRefreshConditionSetWithoutRefreshTime() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		v.setRefreshConditionName("canRefresh");
		// no refreshTimeInSeconds → MetaDataException
		assertThrows(MetaDataException.class, () -> v.convert("TestModule.TestDoc.edit.xml"));
	}

	@Test
	void convertThrowsWhenRefreshActionSetWithoutRefreshTime() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		v.setRefreshActionName("MyAction");
		// no refreshTimeInSeconds → MetaDataException
		assertThrows(MetaDataException.class, () -> v.convert("TestModule.TestDoc.edit.xml"));
	}

	@Test
	void convertSetsRefreshTimeWhenProvided() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		v.setRefreshTimeInSeconds(Integer.valueOf(60));
		ViewImpl result = v.convert("TestModule.TestDoc.edit.xml");
		assertEquals(Integer.valueOf(60), result.getRefreshTimeInSeconds());
	}

	@Test
	void convertThrowsWhenCustomActionHasNoClassName() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		Actions actions = new Actions();
		CustomAction ca = new CustomAction();
		// no className set → resourceName is null, implicitName is null → throws
		actions.getActions().add(ca);
		v.setActions(actions);
		assertThrows(MetaDataException.class, () -> v.convert("TestModule.TestDoc.edit.xml"));
	}

	@Test
	void convertThrowsWhenBizExportActionHasNoClassName() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		Actions actions = new Actions();
		BizExportAction ba = new BizExportAction();
		// no className set → resourceName is null, implicitName=BizExport → throws
		actions.getActions().add(ba);
		v.setActions(actions);
		assertThrows(MetaDataException.class, () -> v.convert("TestModule.TestDoc.edit.xml"));
	}

	@Test
	void convertThrowsWhenReportActionHasNoReportName() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		Actions actions = new Actions();
		ReportAction ra = new ReportAction();
		// no reportName set → resourceName is null, implicitName=Report → throws
		actions.getActions().add(ra);
		v.setActions(actions);
		assertThrows(MetaDataException.class, () -> v.convert("TestModule.TestDoc.edit.xml"));
	}

	@Test
	void convertThrowsWhenDuplicateActionName() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		Actions actions = new Actions();
		SaveAction sa1 = new SaveAction();
		SaveAction sa2 = new SaveAction();
		actions.getActions().add(sa1);
		actions.getActions().add(sa2);
		v.setActions(actions);
		// save action has implicit name "Save" — adding it twice causes duplicate
		assertThrows(MetaDataException.class, () -> v.convert("TestModule.TestDoc.edit.xml"));
	}

	@Test
	void convertSucceedsWithSaveAction() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		Actions actions = new Actions();
		SaveAction sa = new SaveAction();
		actions.getActions().add(sa);
		v.setActions(actions);
		ViewImpl result = v.convert("TestModule.TestDoc.edit.xml");
		assertNotNull(result.getAction("Save"));
	}

	@Test
	void convertThrowsWhenRefreshActionNotPresentInActions() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		v.setRefreshTimeInSeconds(Integer.valueOf(30));
		v.setRefreshActionName("NonExistentAction");
		// no actions defined that match NonExistentAction → throws
		assertThrows(MetaDataException.class, () -> v.convert("TestModule.TestDoc.edit.xml"));
	}

	@Test
	void convertThrowsWhenParameterMissingFromBinding() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		ViewParameter param = new ViewParameter();
		// fromBinding null → throws
		param.setBoundTo("targetBinding");
		v.getParameters().add(param);
		assertThrows(MetaDataException.class, () -> v.convert("TestModule.TestDoc.edit.xml"));
	}

	@Test
	void convertThrowsWhenParameterMissingBoundTo() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		ViewParameter param = new ViewParameter();
		param.setFromBinding("sourceBinding");
		// boundTo null → throws
		v.getParameters().add(param);
		assertThrows(MetaDataException.class, () -> v.convert("TestModule.TestDoc.edit.xml"));
	}

	@Test
	void convertSucceedsWithValidParameters() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		ViewParameter param = new ViewParameter();
		param.setFromBinding("sourceBinding");
		param.setBoundTo("targetBinding");
		v.getParameters().add(param);
		ViewImpl result = v.convert("TestModule.TestDoc.edit.xml");
		assertEquals(1, result.getParameters().size());
		assertEquals("sourceBinding", result.getParameters().get(0).getFromBinding());
	}

	@Test
	void convertCustomActionWithClassNameSucceeds() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		v.setTitle("My View");
		Actions actions = new Actions();
		CustomAction ca = new CustomAction();
		ca.setClassName("com.example.MyAction");
		ca.setDisplayName("My Action");
		actions.getActions().add(ca);
		v.setActions(actions);
		ViewImpl result = v.convert("TestModule.TestDoc.edit.xml");
		assertNotNull(result.getAction("com.example.MyAction"));
	}
}
