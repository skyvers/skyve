package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.actions.AddAction;
import org.skyve.impl.metadata.repository.view.actions.BizExportAction;
import org.skyve.impl.metadata.repository.view.actions.BizImportAction;
import org.skyve.impl.metadata.repository.view.actions.CancelAction;
import org.skyve.impl.metadata.repository.view.actions.CustomAction;
import org.skyve.impl.metadata.repository.view.actions.DefaultsAction;
import org.skyve.impl.metadata.repository.view.actions.DeleteAction;
import org.skyve.impl.metadata.repository.view.actions.NewAction;
import org.skyve.impl.metadata.repository.view.actions.OKAction;
import org.skyve.impl.metadata.repository.view.actions.RemoveAction;
import org.skyve.impl.metadata.repository.view.actions.ReportAction;
import org.skyve.impl.metadata.repository.view.actions.SaveAction;
import org.skyve.impl.metadata.repository.view.actions.ZoomOutAction;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.Action.ActionShow;
import org.skyve.report.ReportFormat;

class ActionImplTest {

	private ActionImpl action;

	@BeforeEach
	void setUp() {
		action = new ActionImpl();
	}

	@Test
	void getNameReturnsNameWhenSet() {
		action.setName("MyAction");
		assertEquals("MyAction", action.getName());
	}

	@Test
	void getNameFallsBackToResourceName() {
		action.setResourceName("MyResource");
		assertEquals("MyResource", action.getName());
	}

	@Test
	void getNameFallsBackToImplicitName() {
		action.setImplicitName(ImplicitActionName.Save);
		assertEquals("Save", action.getName());
	}

	@Test
	void displayNameRoundTrip() {
		action.setDisplayName("Save Document");
		assertEquals("Save Document", action.getDisplayName());
	}

	@Test
	void toolTipRoundTrip() {
		action.setToolTip("Click to save");
		assertEquals("Click to save", action.getToolTip());
	}

	@Test
	void relativeIconFileNameRoundTrip() {
		action.setRelativeIconFileName("icons/save.png");
		assertEquals("icons/save.png", action.getRelativeIconFileName());
	}

	@Test
	void iconStyleClassRoundTrip() {
		action.setIconStyleClass("fa fa-save");
		assertEquals("fa fa-save", action.getIconStyleClass());
	}

	@Test
	void confirmationTextRoundTrip() {
		action.setConfirmationText("Are you sure?");
		assertEquals("Are you sure?", action.getConfirmationText());
	}

	@Test
	void clientValidationDefaultIsTrue() {
		assertTrue(Boolean.TRUE.equals(action.getClientValidation()));
	}

	@Test
	void clientValidationRoundTrip() {
		action.setClientValidation(Boolean.FALSE);
		assertEquals(Boolean.FALSE, action.getClientValidation());
	}

	@Test
	void inActionPanelDefaultIsTrue() {
		assertTrue(Boolean.TRUE.equals(action.getInActionPanel()));
	}

	@Test
	void inActionPanelRoundTrip() {
		action.setInActionPanel(Boolean.FALSE);
		assertEquals(Boolean.FALSE, action.getInActionPanel());
	}

	@Test
	void showDefaultIsBoth() {
		assertEquals(ActionShow.both, action.getShow());
	}

	@Test
	void showRoundTrip() {
		action.setShow(ActionShow.icon);
		assertEquals(ActionShow.icon, action.getShow());
	}

	@Test
	void invisibleConditionNameRoundTrip() {
		action.setInvisibleConditionName("notVisible");
		assertEquals("notVisible", action.getInvisibleConditionName());
	}

	@Test
	void visibleConditionNameNegatesAndStoresAsInvisible() {
		action.setVisibleConditionName("visible");
		assertEquals("notVisible", action.getInvisibleConditionName());
	}

	@Test
	void disabledConditionNameRoundTrip() {
		action.setDisabledConditionName("disabled");
		assertEquals("disabled", action.getDisabledConditionName());
	}

	@Test
	void enabledConditionNameNegatesAndStoresAsDisabled() {
		action.setEnabledConditionName("enabled");
		assertEquals("notEnabled", action.getDisabledConditionName());
	}

	@Test
	void getParametersReturnsMutableList() {
		List<org.skyve.metadata.view.widget.bound.Parameter> params = action.getParameters();
		assertNotNull(params);
		assertTrue(params instanceof ArrayList);
	}

	@Test
	void propertiesRoundTrip() {
		Map<String, String> props = new TreeMap<>();
		props.put("key", "value");
		action.setProperties(props);
		assertSame(props, action.getProperties());
	}

	@Test
	void resourceNameRoundTrip() {
		action.setResourceName("com.example.MyAction");
		assertEquals("com.example.MyAction", action.getResourceName());
	}

	@Test
	void toRepositoryActionCustomAction() {
		action.setName("myAction");
		action.setResourceName("com.example.MyAction");
		org.skyve.impl.metadata.repository.view.actions.ActionMetaData result = action.toRepositoryAction();
		assertNotNull(result);
		assertTrue(result instanceof CustomAction);
	}

	@Test
	void toRepositoryActionSave() {
		action.setImplicitName(ImplicitActionName.Save);
		org.skyve.impl.metadata.repository.view.actions.ActionMetaData result = action.toRepositoryAction();
		assertTrue(result instanceof SaveAction);
	}

	@Test
	void toRepositoryActionNew() {
		action.setImplicitName(ImplicitActionName.New);
		assertTrue(action.toRepositoryAction() instanceof NewAction);
	}

	@Test
	void toRepositoryActionOK() {
		action.setImplicitName(ImplicitActionName.OK);
		assertTrue(action.toRepositoryAction() instanceof OKAction);
	}

	@Test
	void toRepositoryActionCancel() {
		action.setImplicitName(ImplicitActionName.Cancel);
		assertTrue(action.toRepositoryAction() instanceof CancelAction);
	}

	@Test
	void toRepositoryActionDelete() {
		action.setImplicitName(ImplicitActionName.Delete);
		assertTrue(action.toRepositoryAction() instanceof DeleteAction);
	}

	@Test
	void toRepositoryActionAdd() {
		action.setImplicitName(ImplicitActionName.Add);
		assertTrue(action.toRepositoryAction() instanceof AddAction);
	}

	@Test
	void toRepositoryActionRemove() {
		action.setImplicitName(ImplicitActionName.Remove);
		assertTrue(action.toRepositoryAction() instanceof RemoveAction);
	}

	@Test
	void toRepositoryActionSetsDisplayName() {
		action.setImplicitName(ImplicitActionName.Save);
		action.setDisplayName("My Save");
		org.skyve.impl.metadata.repository.view.actions.ActionMetaData result = action.toRepositoryAction();
		assertEquals("My Save", result.getDisplayName());
	}

	@Test
	void toRepositoryActionSetsConfirmationText() {
		action.setImplicitName(ImplicitActionName.Delete);
		action.setConfirmationText("Really delete?");
		org.skyve.impl.metadata.repository.view.actions.ActionMetaData result = action.toRepositoryAction();
		assertEquals("Really delete?", result.getConfirmationText());
	}

	@Test
	void implicitNameRoundTrip() {
		action.setImplicitName(ImplicitActionName.ZoomOut);
		assertEquals(ImplicitActionName.ZoomOut, action.getImplicitName());
	}

	@Test
	void toRepositoryActionNullResourceNameForReport() {
		// Set up a Report action without resourceName — falls back to getName()
		action.setImplicitName(ImplicitActionName.Report);
		action.setName("MyReport");
		org.skyve.impl.metadata.repository.view.actions.ActionMetaData result = action.toRepositoryAction();
		assertNotNull(result);
		assertTrue(result instanceof org.skyve.impl.metadata.repository.view.actions.ReportAction);
	}

	@Test
	void nullResourceNameGetterReturnsNull() {
		assertNull(action.getResourceName());
	}

	// ---- default interface methods on Action (coverage for org.skyve.metadata.view.Action) ----

	@Test
	void localisedDisplayNameReturnsValueWhenDisplayNameSet() {
		action.setDisplayName("Save Document");
		assertNotNull(action.getLocalisedDisplayName());
	}

	@Test
	void localisedDisplayNameReturnsNullWhenDisplayNameNull() {
		assertNull(action.getLocalisedDisplayName());
	}

	@Test
	void localisedConfirmationTextReturnsValueWhenSet() {
		action.setConfirmationText("Are you sure?");
		assertNotNull(action.getLocalisedConfirmationText());
	}

	@Test
	void localisedConfirmationTextReturnsNullWhenNotSet() {
		assertNull(action.getLocalisedConfirmationText());
	}

	@Test
	void localisedToolTipReturnsValueWhenSet() {
		action.setToolTip("Click to save");
		assertNotNull(action.getLocalisedToolTip());
	}

	@Test
	void localisedToolTipReturnsNullWhenNotSet() {
		assertNull(action.getLocalisedToolTip());
	}

	@Test
	void toRepositoryActionBizExport() {
		action.setImplicitName(ImplicitActionName.BizExport);
		assertTrue(action.toRepositoryAction() instanceof BizExportAction);
	}

	@Test
	void toRepositoryActionBizImport() {
		action.setImplicitName(ImplicitActionName.BizImport);
		assertTrue(action.toRepositoryAction() instanceof BizImportAction);
	}

	@Test
	void toRepositoryActionDefaults() {
		action.setImplicitName(ImplicitActionName.DEFAULTS);
		assertTrue(action.toRepositoryAction() instanceof DefaultsAction);
	}

	@Test
	void toRepositoryActionZoomOut() {
		action.setImplicitName(ImplicitActionName.ZoomOut);
		assertTrue(action.toRepositoryAction() instanceof ZoomOutAction);
	}

	@Test
	void toRepositoryActionSetsClassNameOnClassAction() {
		// BizExportAction is a ClassAction
		action.setImplicitName(ImplicitActionName.BizExport);
		action.setResourceName("com.example.MyExport");
		org.skyve.impl.metadata.repository.view.actions.ActionMetaData result = action.toRepositoryAction();
		assertTrue(result instanceof BizExportAction);
		assertEquals("com.example.MyExport", ((BizExportAction) result).getClassName());
	}

	@Test
	void toRepositoryActionSetsInActionPanelFalseOnPositionableAction() {
		// SaveAction is a PositionableAction
		action.setImplicitName(ImplicitActionName.Save);
		action.setInActionPanel(Boolean.FALSE);
		org.skyve.impl.metadata.repository.view.actions.ActionMetaData result = action.toRepositoryAction();
		assertTrue(result instanceof SaveAction);
		assertEquals(Boolean.FALSE, ((SaveAction) result).getInActionPanel());
	}

	@Test
	void toRepositoryActionCopiesParametersOnParameterizableAction() {
		// ReportAction is a ParameterizableAction
		action.setImplicitName(ImplicitActionName.Report);
		ParameterImpl p = new ParameterImpl();
		p.setName("myParam");
		p.setValue("myValue");
		action.getParameters().add(p);
		org.skyve.impl.metadata.repository.view.actions.ActionMetaData result = action.toRepositoryAction();
		assertTrue(result instanceof ReportAction);
		assertEquals(1, ((ReportAction) result).getParameters().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void toRepositoryActionReportSetsModuleAndDocumentAndFormat() {
		ActionImpl act = new ActionImpl();
		act.setImplicitName(ImplicitActionName.Report);
		act.setResourceName("myReport");

		ParameterImpl modParam = new ParameterImpl();
		modParam.setName(AbstractWebContext.MODULE_NAME);
		modParam.setValue("admin");
		act.getParameters().add(modParam);

		ParameterImpl docParam = new ParameterImpl();
		docParam.setName(AbstractWebContext.DOCUMENT_NAME);
		docParam.setValue("User");
		act.getParameters().add(docParam);

		ParameterImpl fmtParam = new ParameterImpl();
		fmtParam.setName(AbstractWebContext.REPORT_FORMAT);
		fmtParam.setValue(ReportFormat.pdf.name());
		act.getParameters().add(fmtParam);

		org.skyve.impl.metadata.repository.view.actions.ActionMetaData result = act.toRepositoryAction();
		assertTrue(result instanceof ReportAction);
		assertEquals("admin", ((ReportAction) result).getModuleName());
		assertEquals("User", ((ReportAction) result).getDocumentName());
		assertEquals(ReportFormat.pdf, ((ReportAction) result).getReportFormat());
	}

	@Test
	@SuppressWarnings("static-method")
	void getServerSideActionThrowsWhenResourceNameIsNull() {
		ActionImpl act = new ActionImpl();
		act.setImplicitName(ImplicitActionName.Save);
		// resourceName is null → should throw
		assertThrows(IllegalStateException.class, () -> act.getServerSideAction(null, null));
	}
}
