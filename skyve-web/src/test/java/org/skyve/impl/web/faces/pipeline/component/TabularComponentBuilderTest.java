package org.skyve.impl.web.faces.pipeline.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.primefaces.behavior.ajax.AjaxBehavior;
import org.primefaces.behavior.confirm.ConfirmBehavior;
import org.primefaces.component.column.Column;
import org.primefaces.component.colorpicker.ColorPicker;
import org.primefaces.component.commandbutton.CommandButton;
import org.primefaces.component.commandlink.CommandLink;
import org.primefaces.component.contextmenu.ContextMenu;
import org.primefaces.component.datatable.DataTable;
import org.primefaces.component.graphicimage.GraphicImage;
import org.primefaces.component.inputtext.InputText;
import org.primefaces.component.inputtextarea.InputTextarea;
import org.primefaces.component.menuitem.UIMenuItem;
import org.primefaces.component.outputlabel.OutputLabel;
import org.primefaces.component.panel.Panel;
import org.primefaces.component.picklist.PickList;
import org.primefaces.component.password.Password;
import org.primefaces.component.selectonemenu.SelectOneMenu;
import org.primefaces.component.selectoneradio.SelectOneRadio;
import org.primefaces.component.signature.Signature;
import org.primefaces.component.spinner.Spinner;
import org.primefaces.component.tabview.Tab;
import org.primefaces.component.tabview.TabView;
import org.primefaces.component.texteditor.TextEditor;
import org.primefaces.component.toolbar.Toolbar;
import org.primefaces.component.autocomplete.AutoComplete;
import org.skyve.impl.metadata.controller.CustomisationsStaticSingleton;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.KeyboardType;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder.EventSourceComponent;
import org.skyve.impl.web.faces.models.SkyveLazyDataModel;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.controller.Customisations;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.model.list.ListModel;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.el.MethodExpression;
import jakarta.el.ValueExpression;
import jakarta.faces.application.Application;
import jakarta.faces.component.UICommand;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.component.UIInput;
import jakarta.faces.component.UIOutput;
import jakarta.faces.component.UISelectItems;
import jakarta.faces.component.html.HtmlInputHidden;
import jakarta.faces.component.html.HtmlInputText;
import jakarta.faces.component.html.HtmlOutputLink;
import jakarta.faces.component.html.HtmlOutputText;
import jakarta.faces.component.html.HtmlPanelGrid;
import jakarta.faces.component.html.HtmlPanelGroup;
import jakarta.faces.convert.Converter;
import jakarta.faces.context.FacesContext;
import org.primefaces.model.DualListModel;

class TabularComponentBuilderTest {

	private static ExpressionFactory mockExpressionFactory;
	private static Application mockApplication;

	private static abstract class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext facesContext) {
			setCurrentInstance(facesContext);
		}
	}

	private static final class NoOpTabularComponentBuilder extends TabularComponentBuilder {
		// Deliberately empty - inherits all behaviour from TabularComponentBuilder.
		void setManagedBeanForTest(FacesView managedBeanForTest) {
			this.managedBean = managedBeanForTest;
		}

		Panel invokePanelForTest(String title, String invisible, Integer pixelWidth, Collapsible collapsible, String widgetId) {
			return panel(title, invisible, pixelWidth, collapsible, widgetId);
		}

		void invokeZoomInActionExpressionForTest(String referenceBinding, UICommand command) {
			zoomInActionExpression(referenceBinding, command);
		}

		void invokeSetValueOrValueExpressionForTest(String value, java.util.function.Consumer<String> valueSetter, String valueExpressionName, UIComponent component) {
			setValueOrValueExpression(value, valueSetter, valueExpressionName, component);
		}
	}

	private static final class CapturingDelegationBuilder extends TabularComponentBuilder {
		UIComponent delegatedActionButtonResult = new HtmlPanelGroup();
		UIComponent delegatedUploadButtonResult = new HtmlPanelGroup();
		CommandLink delegatedActionLinkResult = mock(CommandLink.class);
		CommandLink delegatedDownloadLinkResult = mock(CommandLink.class);

		String actionTitle;
		String actionIconStyleClass;
		String actionTooltip;
		ImplicitActionName actionImplicitName;
		String actionName;
		boolean actionInline;
		String actionDataWidgetBinding;
		String actionDataWidgetVar;
		Integer actionPixelWidth;
		Integer actionPixelHeight;
		String actionConfirmationText;
		String actionDisabled;
		String actionFormDisabled;
		String actionInvisible;
		String actionProcessOverride;
		String actionUpdateOverride;
		boolean actionCanDelete;

		String uploadTitle;
		String uploadIconStyleClass;
		String uploadTooltip;
		String uploadActionName;
		Integer uploadPixelWidth;
		Integer uploadPixelHeight;
		Boolean uploadClientValidation;
		String uploadConfirmationText;
		String uploadDisabled;
		String uploadFormDisabled;
		String uploadInvisible;
		boolean uploadUseDialog;

		String linkTitle;
		String linkTooltip;
		ImplicitActionName linkImplicitName;
		String linkActionName;
		boolean linkInline;
		String linkDataWidgetBinding;
		String linkDataWidgetVar;
		Integer linkPixelWidth;
		Integer linkPixelHeight;
		Boolean linkClientValidation;
		String linkConfirmationText;
		String linkDisabled;
		String linkFormDisabled;
		String linkInvisible;
		String linkProcessOverride;
		String linkUpdateOverride;

		String downloadLinkTitle;
		String downloadLinkTooltip;
		String downloadLinkActionName;
		String downloadLinkDataWidgetBinding;
		String downloadLinkDataWidgetVar;
		Integer downloadLinkPixelWidth;
		String downloadLinkConfirmationText;
		String downloadLinkDisabled;
		String downloadLinkFormDisabled;
		String downloadLinkInvisible;
		String downloadLinkProcessOverride;
		String downloadLinkUpdateOverride;

		void setManagedBeanForTest(FacesView managedBeanForTest) {
			this.managedBean = managedBeanForTest;
		}

		@Override
		protected CommandButton actionButton(String title,
												String iconStyleClass,
												String tooltip,
												ImplicitActionName implicitActionName,
												String actionName,
												boolean inline,
												String dataWidgetBinding,
												String dataWidgetVar,
												Integer pixelWidth,
												Integer pixelHeight,
												String confirmationText,
												String disabled,
												String formDisabled,
												String invisible,
												String processOverride,
												String updateOverride,
												boolean canDelete) {
			this.actionTitle = title;
			this.actionIconStyleClass = iconStyleClass;
			this.actionTooltip = tooltip;
			this.actionImplicitName = implicitActionName;
			this.actionName = actionName;
			this.actionInline = inline;
			this.actionDataWidgetBinding = dataWidgetBinding;
			this.actionDataWidgetVar = dataWidgetVar;
			this.actionPixelWidth = pixelWidth;
			this.actionPixelHeight = pixelHeight;
			this.actionConfirmationText = confirmationText;
			this.actionDisabled = disabled;
			this.actionFormDisabled = formDisabled;
			this.actionInvisible = invisible;
			this.actionProcessOverride = processOverride;
			this.actionUpdateOverride = updateOverride;
			this.actionCanDelete = canDelete;
			return (CommandButton) delegatedActionButtonResult;
		}

		@Override
		protected UIComponent uploadButton(String title,
											String iconStyleClass,
											String tooltip,
											String actionName,
											Integer pixelWidth,
											Integer pixelHeight,
											Boolean clientValidation,
											String confirmationText,
											String disabled,
											String formDisabled,
											String invisible,
											boolean useDialog) {
			this.uploadTitle = title;
			this.uploadIconStyleClass = iconStyleClass;
			this.uploadTooltip = tooltip;
			this.uploadActionName = actionName;
			this.uploadPixelWidth = pixelWidth;
			this.uploadPixelHeight = pixelHeight;
			this.uploadClientValidation = clientValidation;
			this.uploadConfirmationText = confirmationText;
			this.uploadDisabled = disabled;
			this.uploadFormDisabled = formDisabled;
			this.uploadInvisible = invisible;
			this.uploadUseDialog = useDialog;
			return delegatedUploadButtonResult;
		}

		@Override
		protected CommandLink actionLink(String title,
											String tooltip,
											ImplicitActionName implicitActionName,
											String actionName,
											boolean inline,
											String dataWidgetBinding,
											String dataWidgetVar,
											Integer pixelWidth,
											Integer pixelHeight,
											Boolean clientValidation,
											String confirmationText,
											String disabled,
											String formDisabled,
											String invisible,
											String processOverride,
											String updateOverride) {
			this.linkTitle = title;
			this.linkTooltip = tooltip;
			this.linkImplicitName = implicitActionName;
			this.linkActionName = actionName;
			this.linkInline = inline;
			this.linkDataWidgetBinding = dataWidgetBinding;
			this.linkDataWidgetVar = dataWidgetVar;
			this.linkPixelWidth = pixelWidth;
			this.linkPixelHeight = pixelHeight;
			this.linkClientValidation = clientValidation;
			this.linkConfirmationText = confirmationText;
			this.linkDisabled = disabled;
			this.linkFormDisabled = formDisabled;
			this.linkInvisible = invisible;
			this.linkProcessOverride = processOverride;
			this.linkUpdateOverride = updateOverride;
			return delegatedActionLinkResult;
		}

		@Override
		protected CommandLink downloadLink(String title,
											String tooltip,
											String actionName,
											String dataWidgetBinding,
											String dataWidgetVar,
											Integer pixelWidth,
											String confirmationText,
											String disabled,
											String formDisabled,
											String invisible,
											String processOverride,
											String updateOverride) {
			this.downloadLinkTitle = title;
			this.downloadLinkTooltip = tooltip;
			this.downloadLinkActionName = actionName;
			this.downloadLinkDataWidgetBinding = dataWidgetBinding;
			this.downloadLinkDataWidgetVar = dataWidgetVar;
			this.downloadLinkPixelWidth = pixelWidth;
			this.downloadLinkConfirmationText = confirmationText;
			this.downloadLinkDisabled = disabled;
			this.downloadLinkFormDisabled = formDisabled;
			this.downloadLinkInvisible = invisible;
			this.downloadLinkProcessOverride = processOverride;
			this.downloadLinkUpdateOverride = updateOverride;
			return delegatedDownloadLinkResult;
		}
	}

	private static final class ListGridActionColumnBuilder extends TabularComponentBuilder {
		private final UIComponent filterToggle;
		private final UIComponent zoomButton;

		ListGridActionColumnBuilder(UIComponent filterToggle, UIComponent zoomButton) {
			this.filterToggle = filterToggle;
			this.zoomButton = zoomButton;
		}

		void setManagedBeanForTest(FacesView managedBeanForTest) {
			this.managedBean = managedBeanForTest;
		}

		@Override
		protected UIComponent createDataTableFilterToggle(String dataTableId) {
			return filterToggle;
		}

		@Override
		protected UIComponent createListGridZoomButton(String zoomDisabledConditionName, Map<String, String> properties) {
			return zoomButton;
		}
	}

	private static final class CapturingInputDelegationBuilder extends TabularComponentBuilder {
		UIInput delegatedCheckBoxResult = mock(UIInput.class);
		ColorPicker delegatedColourPickerResult = mock(ColorPicker.class);
		AutoComplete delegatedLookupDescriptionResult = mock(AutoComplete.class);
		Password delegatedPasswordResult = mock(Password.class);
		InputTextarea delegatedTextAreaResult = mock(InputTextarea.class);
		InputText delegatedTextFieldResult = mock(InputText.class);
		AutoComplete delegatedCompleteResult = mock(AutoComplete.class);

		String checkBoxDataWidgetVar;
		String checkBoxBinding;
		String checkBoxTitle;
		String checkBoxRequiredMessage;
		String checkBoxDisabled;
		String checkBoxFormDisabled;
		boolean checkBoxTriState;

		String colourPickerDataWidgetVar;
		String colourPickerBinding;
		String colourPickerTitle;
		String colourPickerRequiredMessage;
		HorizontalAlignment colourPickerTextAlignment;
		String colourPickerDisabled;
		String colourPickerFormDisabled;
		Integer colourPickerPixelWidth;

		String lookupDataWidgetVar;
		String lookupBinding;
		String lookupTitle;
		String lookupRequiredMessage;
		HorizontalAlignment lookupTextAlignment;
		String lookupDisabled;
		String lookupFormDisabled;
		String lookupDisplayBinding;
		QueryDefinition lookupQuery;
		Integer lookupPixelWidth;

		String passwordDataWidgetVar;
		String passwordBinding;
		String passwordTitle;
		String passwordRequiredMessage;
		HorizontalAlignment passwordTextAlignment;
		String passwordDisabled;
		String passwordFormDisabled;
		Integer passwordPixelWidth;

		String textAreaDataWidgetVar;
		String textAreaBinding;
		String textAreaTitle;
		String textAreaRequiredMessage;
		HorizontalAlignment textAreaTextAlignment;
		boolean textAreaReadonly;
		String textAreaDisabled;
		String textAreaFormDisabled;
		Integer textAreaMaxLength;
		Integer textAreaPixelWidth;
		Integer textAreaPixelHeight;

		String textFieldDataWidgetVar;
		String textFieldBinding;
		String textFieldTitle;
		String textFieldRequiredMessage;
		HorizontalAlignment textFieldTextAlignment;
		boolean textFieldReadonly;
		String textFieldDisabled;
		String textFieldFormDisabled;
		Integer textFieldMaxLength;
		jakarta.faces.convert.Converter<?> textFieldConverter;
		KeyboardType textFieldKeyboardType;
		Integer textFieldPixelWidth;

		String completeDataWidgetVar;
		String completeBinding;
		String completeTitle;
		String completeRequiredMessage;
		HorizontalAlignment completeTextAlignment;
		String completeDisabled;
		Integer completeLength;
		String completeFormDisabled;
		CompleteType completeType;
		KeyboardType completeKeyboardType;
		Integer completePixelWidth;

		@Override
		protected UIInput checkbox(String dataWidgetVar,
									String binding,
									String title,
									String requiredMessage,
									String disabled,
									String formDisabled,
									boolean triState) {
			this.checkBoxDataWidgetVar = dataWidgetVar;
			this.checkBoxBinding = binding;
			this.checkBoxTitle = title;
			this.checkBoxRequiredMessage = requiredMessage;
			this.checkBoxDisabled = disabled;
			this.checkBoxFormDisabled = formDisabled;
			this.checkBoxTriState = triState;
			return delegatedCheckBoxResult;
		}

		@Override
		protected ColorPicker colourPicker(String dataWidgetVar,
											String binding,
											String title,
											String requiredMessage,
											HorizontalAlignment textAlignment,
											String disabled,
											String formDisabled,
											Integer pixelWidth) {
			this.colourPickerDataWidgetVar = dataWidgetVar;
			this.colourPickerBinding = binding;
			this.colourPickerTitle = title;
			this.colourPickerRequiredMessage = requiredMessage;
			this.colourPickerTextAlignment = textAlignment;
			this.colourPickerDisabled = disabled;
			this.colourPickerFormDisabled = formDisabled;
			this.colourPickerPixelWidth = pixelWidth;
			return delegatedColourPickerResult;
		}

		@Override
		protected AutoComplete lookupDescription(String dataWidgetVar,
													String binding,
													String title,
													String requiredMessage,
													HorizontalAlignment textAlignment,
													String disabled,
													String formDisabled,
													String displayBinding,
													QueryDefinition query,
													List<org.skyve.metadata.view.widget.FilterParameter> filterParameters,
													List<org.skyve.metadata.view.widget.bound.Parameter> parameters,
													Integer pixelWidth,
													boolean dontDisplay) {
			this.lookupDataWidgetVar = dataWidgetVar;
			this.lookupBinding = binding;
			this.lookupTitle = title;
			this.lookupRequiredMessage = requiredMessage;
			this.lookupTextAlignment = textAlignment;
			this.lookupDisabled = disabled;
			this.lookupFormDisabled = formDisabled;
			this.lookupDisplayBinding = displayBinding;
			this.lookupQuery = query;
			this.lookupPixelWidth = pixelWidth;
			return delegatedLookupDescriptionResult;
		}

		@Override
		protected Password password(String dataWidgetVar,
									String binding,
									String title,
									String requiredMessage,
									HorizontalAlignment textAlignment,
									String disabled,
									String formDisabled,
									Integer pixelWidth) {
			this.passwordDataWidgetVar = dataWidgetVar;
			this.passwordBinding = binding;
			this.passwordTitle = title;
			this.passwordRequiredMessage = requiredMessage;
			this.passwordTextAlignment = textAlignment;
			this.passwordDisabled = disabled;
			this.passwordFormDisabled = formDisabled;
			this.passwordPixelWidth = pixelWidth;
			return delegatedPasswordResult;
		}

		@Override
		protected InputTextarea textArea(String dataWidgetVar,
											String binding,
											String title,
											String requiredMessage,
											HorizontalAlignment textAlignment,
											boolean readonly,
											String disabled,
											String formDisabled,
											Integer maxLength,
											Integer pixelWidth,
											Integer pixelHeight) {
			this.textAreaDataWidgetVar = dataWidgetVar;
			this.textAreaBinding = binding;
			this.textAreaTitle = title;
			this.textAreaRequiredMessage = requiredMessage;
			this.textAreaTextAlignment = textAlignment;
			this.textAreaReadonly = readonly;
			this.textAreaDisabled = disabled;
			this.textAreaFormDisabled = formDisabled;
			this.textAreaMaxLength = maxLength;
			this.textAreaPixelWidth = pixelWidth;
			this.textAreaPixelHeight = pixelHeight;
			return delegatedTextAreaResult;
		}

		@Override
		protected InputText textField(String dataWidgetVar,
										String binding,
										String title,
										String requiredMessage,
										HorizontalAlignment textAlignment,
										boolean readonly,
										String disabled,
										String formDisabled,
										Integer maxLength,
										org.skyve.domain.types.converters.Format.TextCase textCase,
										jakarta.faces.convert.Converter<?> converter,
										KeyboardType keyboardType,
										Integer pixelWidth) {
			this.textFieldDataWidgetVar = dataWidgetVar;
			this.textFieldBinding = binding;
			this.textFieldTitle = title;
			this.textFieldRequiredMessage = requiredMessage;
			this.textFieldTextAlignment = textAlignment;
			this.textFieldReadonly = readonly;
			this.textFieldDisabled = disabled;
			this.textFieldFormDisabled = formDisabled;
			this.textFieldMaxLength = maxLength;
			this.textFieldConverter = converter;
			this.textFieldKeyboardType = keyboardType;
			this.textFieldPixelWidth = pixelWidth;
			return delegatedTextFieldResult;
		}

		@Override
		protected AutoComplete complete(String dataWidgetVar,
											String binding,
											String title,
											String requiredMessage,
											HorizontalAlignment textAlignment,
											String disabled,
											Integer length,
											String formDisabled,
											CompleteType complete,
											KeyboardType keyboardType,
											Integer pixelWidth) {
			this.completeDataWidgetVar = dataWidgetVar;
			this.completeBinding = binding;
			this.completeTitle = title;
			this.completeRequiredMessage = requiredMessage;
			this.completeTextAlignment = textAlignment;
			this.completeDisabled = disabled;
			this.completeLength = length;
			this.completeFormDisabled = formDisabled;
			this.completeType = complete;
			this.completeKeyboardType = keyboardType;
			this.completePixelWidth = pixelWidth;
			return delegatedCompleteResult;
		}
	}

	private static final class CapturingListBuilder extends TabularComponentBuilder {
		ListModel<? extends org.skyve.domain.Bean> capturedModel;
		List<UIComponent> capturedChildren;
		boolean capturedShowFilter;
		String capturedWidgetVar;
		String capturedUxui;

		void setManagedBeanForTest(FacesView managedBeanForTest) {
			this.managedBean = managedBeanForTest;
		}

		@Override
		protected void addListGridDataColumns(ListModel<? extends org.skyve.domain.Bean> model,
												List<UIComponent> children,
												boolean showFilter,
												String listGridWidgetVar,
												String uxui) {
			this.capturedModel = model;
			this.capturedChildren = children;
			this.capturedShowFilter = showFilter;
			this.capturedWidgetVar = listGridWidgetVar;
			this.capturedUxui = uxui;
		}
	}

	private static final class CapturingListGridBuilder extends TabularComponentBuilder {
		ListModel<? extends org.skyve.domain.Bean> capturedModel;
		List<UIComponent> capturedChildren;
		boolean capturedShowFilter;
		String capturedWidgetVar;
		String capturedUxui;
		UIComponent actionColumnToReturn = new HtmlPanelGroup();
		boolean actionColumnCalled;
		String actionModuleName;
		String actionDocumentName;
		boolean actionCanCreateDocument;
		boolean actionCreateRendered;
		String[] actionCreateDisabled;
		String actionCreateUrlParams;
		boolean actionZoomRendered;
		String actionZoomDisabledConditionName;
		boolean actionShowFilter;
		String actionParentId;
		Map<String, String> actionProperties;

		void setManagedBeanForTest(FacesView managedBeanForTest) {
			this.managedBean = managedBeanForTest;
		}

		@Override
		protected void addListGridDataColumns(ListModel<? extends org.skyve.domain.Bean> model,
												List<UIComponent> children,
												boolean showFilter,
												String listGridWidgetVar,
												String uxui) {
			this.capturedModel = model;
			this.capturedChildren = children;
			this.capturedShowFilter = showFilter;
			this.capturedWidgetVar = listGridWidgetVar;
			this.capturedUxui = uxui;
		}

		@Override
		protected UIComponent createListGridActionColumn(String moduleName,
															String documentName,
															boolean canCreateDocument,
															boolean createRendered,
															String[] createDisabledConditionNames,
															String createUrlParams,
															boolean zoomRendered,
															String zoomDisabledConditionName,
															boolean showFilter,
															String parentId,
															Map<String, String> properties) {
			this.actionColumnCalled = true;
			this.actionModuleName = moduleName;
			this.actionDocumentName = documentName;
			this.actionCanCreateDocument = canCreateDocument;
			this.actionCreateRendered = createRendered;
			this.actionCreateDisabled = createDisabledConditionNames;
			this.actionCreateUrlParams = createUrlParams;
			this.actionZoomRendered = zoomRendered;
			this.actionZoomDisabledConditionName = zoomDisabledConditionName;
			this.actionShowFilter = showFilter;
			this.actionParentId = parentId;
			this.actionProperties = properties;
			return actionColumnToReturn;
		}
	}

	@BeforeAll
	static void setUpFacesContext() {
		FacesContext facesContext = mock(FacesContext.class);
		mockApplication = mock(Application.class);
		mockExpressionFactory = mock(ExpressionFactory.class);
		ELContext elContext = mock(ELContext.class);

		when(facesContext.getApplication()).thenReturn(mockApplication);
		when(facesContext.getELContext()).thenReturn(elContext);
		when(mockApplication.getExpressionFactory()).thenReturn(mockExpressionFactory);

		FacesContextBridge.setCurrent(facesContext);
	}

	@AfterAll
	static void tearDownFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	@SuppressWarnings("static-method")
	@Test
	void testShortCircuitMethodsReturnExistingComponent() throws Exception {
		TabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		UIComponent existingComponent = new HtmlPanelGroup();
		EventSourceComponent existingEventSource = new EventSourceComponent(new HtmlPanelGroup(), new HtmlInputText());
		List<UIComponent> existingList = new ArrayList<>();

		Set<String> excluded = Set.of("tabPaneScript", "sidebarScript");
		int exercisedMethods = 0;

		for (Method method : TabularComponentBuilder.class.getMethods()) {
			if (! TabularComponentBuilder.class.equals(method.getDeclaringClass())) {
				continue;
			}
			if (method.isSynthetic() || Modifier.isStatic(method.getModifiers())) {
				continue;
			}
			if (excluded.contains(method.getName())) {
				continue;
			}

			Class<?>[] parameterTypes = method.getParameterTypes();
			if (parameterTypes.length == 0) {
				continue;
			}

			Object expected;
			if (UIComponent.class.isAssignableFrom(parameterTypes[0])) {
				expected = existingComponent;
			}
			else if (EventSourceComponent.class.equals(parameterTypes[0])) {
				expected = existingEventSource;
			}
			else if (List.class.isAssignableFrom(parameterTypes[0])) {
				expected = existingList;
			}
			else {
				continue;
			}

			Object[] args = new Object[parameterTypes.length];
			args[0] = expected;
			for (int i = 1; i < parameterTypes.length; i++) {
				args[i] = defaultValue(parameterTypes[i]);
			}

			try {
				Object actual = method.invoke(builder, args);
				assertSame(expected, actual, method.getName() + " should return the existing instance");
				exercisedMethods++;
			}
			catch (InvocationTargetException e) {
				Throwable cause = e.getCause();
				throw new AssertionError("Method " + method.getName() + " threw " + cause, cause);
			}
		}

		assertTrue(exercisedMethods >= 50, "Expected to exercise many short-circuit methods");
	}

	@SuppressWarnings("static-method")
	@Test
	void testSidebarScriptDefaultWidthBreakpointAndFloating() {
		TabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		Sidebar sidebar = new Sidebar();

		UIOutput result = (UIOutput) builder.sidebarScript(null, sidebar, true, "sb1");
		String script = (String) result.getValue();

		assertNotNull(script);
		assertTrue(script.contains("SKYVE.PF.sidebar('sb1','360px',1280,360,'Create')"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testToolbarsCreatesToolbarWhenComponentsNull() {
		TabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		Toolbar toolbar = new Toolbar();
		when(mockApplication.createComponent(Toolbar.COMPONENT_TYPE)).thenReturn(toolbar);

		List<UIComponent> result = builder.toolbars(null, "tb1");

		assertNotNull(result);
		assertEquals(1, result.size());
		assertSame(toolbar, result.get(0));
		assertEquals("tb1", toolbar.getId());
	}

	@SuppressWarnings("static-method")
	@Test
	void testToolbarsCreatesToolbarWithGeneratedIdWhenWidgetIdNull() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		Toolbar toolbar = new Toolbar();
		when(mockApplication.createComponent(Toolbar.COMPONENT_TYPE)).thenReturn(toolbar);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("generatedToolbarId");
		builder.setManagedBeanForTest(managedBean);

		List<UIComponent> result = builder.toolbars(null, null);

		assertNotNull(result);
		assertEquals(1, result.size());
		assertSame(toolbar, result.get(0));
		assertEquals("generatedToolbarId", toolbar.getId());
	}

	@SuppressWarnings("static-method")
	@Test
	void testTabPaneCreatesHiddenTabViewWhenNoSelectedTabBinding() {
		TabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		TabView tabView = new TabView();
		when(mockApplication.createComponent(TabView.COMPONENT_TYPE)).thenReturn(tabView);
		TabPane tabPane = new TabPane();
		tabPane.setWidgetId("tp1");

		TabView result = (TabView) builder.tabPane(null, tabPane, "admin", "User");

		assertSame(tabView, result);
		assertNotNull(result);
	}

	@SuppressWarnings("static-method")
	@Test
	void testTabPaneUsesGeneratedIdWhenWidgetIdNull() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		TabView tabView = new TabView();
		when(mockApplication.createComponent(TabView.COMPONENT_TYPE)).thenReturn(tabView);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("generatedTabPaneId");
		builder.setManagedBeanForTest(managedBean);

		TabPane tabPane = new TabPane();
		TabView result = (TabView) builder.tabPane(null, tabPane, "admin", "User");

		assertSame(tabView, result);
		assertEquals("generatedTabPaneId", result.getId());
	}

	@SuppressWarnings("static-method")
	@Test
	void testTabCreatesTabComponentWhenNull() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		Tab tabComponent = new Tab();
		when(mockApplication.createComponent(Tab.COMPONENT_TYPE)).thenReturn(tabComponent);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("generatedTabId");
		builder.setManagedBeanForTest(managedBean);

		org.skyve.impl.metadata.view.container.Tab metadataTab = new org.skyve.impl.metadata.view.container.Tab();
		Tab result = (Tab) builder.tab(null, "My Tab", metadataTab);

		assertSame(tabComponent, result);
		assertEquals("generatedTabId", result.getId());
	}

	@SuppressWarnings("static-method")
	@Test
	void testLabelCreatesOutputLabelWhenNull() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		OutputLabel label = new OutputLabel();
		when(mockApplication.createComponent(OutputLabel.COMPONENT_TYPE)).thenReturn(label);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("generatedLabelId");
		builder.setManagedBeanForTest(managedBean);

		OutputLabel result = (OutputLabel) builder.label(null, "A Label");

		assertSame(label, result);
		assertEquals("generatedLabelId", result.getId());
	}

	@SuppressWarnings("static-method")
	@Test
	void testBlurbUsesOutputTextValuePathWhenComponentNull() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		HtmlOutputText outputText = mock(HtmlOutputText.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("blurbId");
		builder.setManagedBeanForTest(managedBean);
		when(mockApplication.createComponent(HtmlOutputText.COMPONENT_TYPE)).thenReturn(outputText);

		Blurb blurb = new Blurb();
		blurb.setEscape(Boolean.FALSE);

		UIComponent result = builder.blurb(null, null, "Hello <b>Skyve</b>", null, blurb);

		assertSame(outputText, result);
		verify(outputText).setId("blurbId");
		verify(outputText).setValue("Hello <b>Skyve</b>");
		verify(outputText).setEscape(false);
	}

	@SuppressWarnings("static-method")
	@Test
	void testBoundLabelUsesBindingExpressionAndEscapesQuotes() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		HtmlOutputText outputText = mock(HtmlOutputText.class);
		ValueExpression valueExpression = mock(ValueExpression.class);
		AtomicReference<String> capturedExpression = new AtomicReference<>();
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("boundLabelId");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(HtmlOutputText.COMPONENT_TYPE)).thenReturn(outputText);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(Object.class))).thenAnswer(invocation -> {
			capturedExpression.set(invocation.getArgument(1, String.class));
			return valueExpression;
		});

		Label label = new Label();
		label.setEscape(Boolean.TRUE);
		UIComponent result = builder.label(null, "row", null, "name'Binding", label);

		assertSame(outputText, result);
		assertNotNull(capturedExpression.get());
		assertTrue(capturedExpression.get().contains("name\\'Binding"));
		verify(outputText).setValueExpression("value", valueExpression);
		verify(outputText).setEscape(false);
	}

	@SuppressWarnings("static-method")
	@Test
	void testBoundLabelBindingWithoutDataWidgetVarUsesCurrentBeanPath() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		HtmlOutputText outputText = mock(HtmlOutputText.class);
		ValueExpression valueExpression = mock(ValueExpression.class);
		AtomicReference<String> capturedExpression = new AtomicReference<>();
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("boundLabelIdNoVar");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(HtmlOutputText.COMPONENT_TYPE)).thenReturn(outputText);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(Object.class))).thenAnswer(invocation -> {
			capturedExpression.set(invocation.getArgument(1, String.class));
			return valueExpression;
		});

		Label label = new Label();
		UIComponent result = builder.label(null, null, null, "status", label);

		assertSame(outputText, result);
		assertNotNull(capturedExpression.get());
		assertTrue(capturedExpression.get().contains("skyve.currentBean"));
		verify(outputText).setValueExpression("value", valueExpression);
	}

	@SuppressWarnings("static-method")
	@Test
	void testPanelWithCollapsibleClosedAddsToggleBehavior() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		Panel panel = mock(Panel.class);
		AjaxBehavior ajax = mock(AjaxBehavior.class);
		MethodExpression methodExpression = mock(MethodExpression.class);

		when(mockApplication.createComponent(Panel.COMPONENT_TYPE)).thenReturn(panel);
		when(mockApplication.createBehavior(AjaxBehavior.BEHAVIOR_ID)).thenReturn(ajax);
		when(mockExpressionFactory.createMethodExpression(any(ELContext.class), anyString(), isNull(), any(Class[].class))).thenReturn(methodExpression);

		builder.invokePanelForTest("Section", null, null, Collapsible.closed, "panel1");

		verify(panel).setHeader("Section");
		verify(panel).setId("panel1");
		verify(panel).setToggleable(true);
		verify(panel).setCollapsed(true);
		verify(ajax).setProcess("@this");
		verify(ajax).setUpdate("@none");
		verify(panel).addClientBehavior(eq("toggle"), eq(ajax));
	}

	@SuppressWarnings("static-method")
	@Test
	void testZoomInActionExpressionSetsNavigateMethodExpression() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		UICommand command = mock(UICommand.class);
		MethodExpression methodExpression = mock(MethodExpression.class);

		when(mockExpressionFactory.createMethodExpression(any(ELContext.class), eq("#{skyve.navigate('orders.customer')}"), isNull(), any(Class[].class))).thenReturn(methodExpression);

		builder.invokeZoomInActionExpressionForTest("orders.customer", command);

		verify(command).setActionExpression(methodExpression);
	}

	@SuppressWarnings("static-method")
	@Test
	void testSetValueOrValueExpressionLiteralUsesSetter() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		AtomicReference<String> setValue = new AtomicReference<>();
		UIComponent component = mock(UIComponent.class);

		builder.invokeSetValueOrValueExpressionForTest("Literal", setValue::set, "header", component);

		assertEquals("Literal", setValue.get());
		verify(component, never()).setValueExpression(anyString(), any(ValueExpression.class));
	}

	@SuppressWarnings("static-method")
	@Test
	void testSetValueOrValueExpressionBindingUsesValueExpression() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		UIComponent component = mock(UIComponent.class);
		ValueExpression valueExpression = mock(ValueExpression.class);

		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(String.class))).thenReturn(valueExpression);

		builder.invokeSetValueOrValueExpressionForTest("{contact.name}", s -> { }, "header", component);

		verify(component).setValueExpression("header", valueExpression);
	}

	@SuppressWarnings("static-method")
	@Test
	void testSetConfirmationAddsConfirmBehavior() throws Exception {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		UIComponentBase component = mock(UIComponentBase.class);
		ConfirmBehavior confirm = mock(ConfirmBehavior.class);
		when(mockApplication.createBehavior(ConfirmBehavior.BEHAVIOR_ID)).thenReturn(confirm);

		Method method = TabularComponentBuilder.class.getDeclaredMethod("setConfirmation", UIComponentBase.class, String.class);
		method.setAccessible(true);
		method.invoke(builder, component, "Proceed?");

		verify(confirm).setMessage("Proceed?");
		verify(confirm).setEscape(false);
		verify(component).addClientBehavior("click", confirm);
	}

	@SuppressWarnings("static-method")
	@Test
	void testZoomInCreatesButtonWithDefaultProcessAndUpdate() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		CommandButton commandButton = mock(CommandButton.class);
		MethodExpression actionExpression = mock(MethodExpression.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("zoomInId");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(CommandButton.COMPONENT_TYPE)).thenReturn(commandButton);
		when(mockExpressionFactory.createMethodExpression(any(ELContext.class), anyString(), isNull(), any(Class[].class))).thenReturn(actionExpression);

		ZoomIn zoomIn = new ZoomIn();
		zoomIn.setBinding("contact");

		UIComponent result = builder.zoomIn(null, "Zoom", "pi pi-search", "Open", zoomIn, null);

		assertSame(commandButton, result);
		verify(commandButton).setValue("Zoom");
		verify(commandButton).setIcon("pi pi-search");
		verify(commandButton).setTitle("Open");
		verify(commandButton).setProcess("@form");
		verify(commandButton).setUpdate("@(form)");
		verify(commandButton).setActionExpression(actionExpression);
	}

	@SuppressWarnings("static-method")
	@Test
	void testZoomInUsesProcessAndUpdateOverrides() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		CommandButton commandButton = mock(CommandButton.class);
		MethodExpression actionExpression = mock(MethodExpression.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("zoomInId2");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(CommandButton.COMPONENT_TYPE)).thenReturn(commandButton);
		when(mockExpressionFactory.createMethodExpression(any(ELContext.class), anyString(), isNull(), any(Class[].class))).thenReturn(actionExpression);

		ZoomIn zoomIn = new ZoomIn();
		zoomIn.setBinding("order.customer");
		zoomIn.getProperties().put("process", "@this");
		zoomIn.getProperties().put("update", "@none");

		UIComponent result = builder.zoomIn(null, "Zoom", "pi pi-search", "Open", zoomIn, null);

		assertSame(commandButton, result);
		verify(commandButton).setProcess("@this");
		verify(commandButton).setUpdate("@none");
		verify(commandButton).setActionExpression(actionExpression);
	}

	@SuppressWarnings("static-method")
	@Test
	void testCheckBoxDelegatesNonShortcutPath() {
		CapturingInputDelegationBuilder builder = new CapturingInputDelegationBuilder();
		CheckBox checkBox = new CheckBox();
		checkBox.setBinding("active");
		checkBox.setTriState(Boolean.TRUE);
		checkBox.setDisabledConditionName("checkDisabled");

		EventSourceComponent result = builder.checkBox(null, "row", checkBox, "formDisabled", "Active", "Required");

		assertSame(builder.delegatedCheckBoxResult, result.getComponent());
		assertSame(builder.delegatedCheckBoxResult, result.getEventSource());
		assertEquals("row", builder.checkBoxDataWidgetVar);
		assertEquals("active", builder.checkBoxBinding);
		assertEquals("Active", builder.checkBoxTitle);
		assertEquals("Required", builder.checkBoxRequiredMessage);
		assertEquals("checkDisabled", builder.checkBoxDisabled);
		assertEquals("formDisabled", builder.checkBoxFormDisabled);
		assertEquals(true, builder.checkBoxTriState);
	}

	@SuppressWarnings("static-method")
	@Test
	void testColourPickerDelegatesNonShortcutPath() {
		CapturingInputDelegationBuilder builder = new CapturingInputDelegationBuilder();
		ColourPicker colourPicker = new ColourPicker();
		colourPicker.setBinding("themeColour");
		colourPicker.setPixelWidth(Integer.valueOf(160));
		colourPicker.setDisabledConditionName("colourDisabled");

		EventSourceComponent result = builder.colourPicker(null,
															"row",
															colourPicker,
															"formDisabledColour",
															"Theme colour",
															null,
															HorizontalAlignment.right);

		assertSame(builder.delegatedColourPickerResult, result.getComponent());
		assertSame(builder.delegatedColourPickerResult, result.getEventSource());
		assertEquals("row", builder.colourPickerDataWidgetVar);
		assertEquals("themeColour", builder.colourPickerBinding);
		assertEquals("Theme colour", builder.colourPickerTitle);
		assertEquals(null, builder.colourPickerRequiredMessage);
		assertEquals(HorizontalAlignment.right, builder.colourPickerTextAlignment);
		assertEquals("colourDisabled", builder.colourPickerDisabled);
		assertEquals("formDisabledColour", builder.colourPickerFormDisabled);
		assertEquals(Integer.valueOf(160), builder.colourPickerPixelWidth);
	}

	@SuppressWarnings("static-method")
	@Test
	void testComboNonShortcutPathAddsSelectItemsChild() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		SelectOneMenu selectOneMenu = mock(SelectOneMenu.class);
		UISelectItems selectItems = mock(UISelectItems.class);
		List<UIComponent> children = new ArrayList<>();
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("comboId", "comboItemsId");
		builder.setManagedBeanForTest(managedBean);

		when(selectOneMenu.getChildren()).thenReturn(children);
		when(mockApplication.createComponent(SelectOneMenu.COMPONENT_TYPE)).thenReturn(selectOneMenu);
		when(mockApplication.createComponent(UISelectItems.COMPONENT_TYPE)).thenReturn(selectItems);

		Combo combo = new Combo();
		combo.setBinding("status");

		EventSourceComponent result = builder.combo(null, "row", combo, null, "Status", null);

		assertSame(selectOneMenu, result.getComponent());
		assertSame(selectOneMenu, result.getEventSource());
		assertSame(selectItems, children.get(0));
	}

	@SuppressWarnings("static-method")
	@Test
	void testContentImageNonShortcutPathWithoutEditableMarkupAddsImageContainer() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		HtmlPanelGrid panelGrid = mock(HtmlPanelGrid.class);
		HtmlPanelGroup imageContainer = mock(HtmlPanelGroup.class);
		GraphicImage image = mock(GraphicImage.class);
		List<UIComponent> panelChildren = new ArrayList<>();
		List<UIComponent> imageChildren = new ArrayList<>();
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("contentImageGrid", "contentImageInner", "contentImageTag");
		builder.setManagedBeanForTest(managedBean);

		when(panelGrid.getId()).thenReturn("contentImageGrid");
		when(panelGrid.getChildren()).thenReturn(panelChildren);
		when(imageContainer.getChildren()).thenReturn(imageChildren);
		when(mockApplication.createComponent(HtmlPanelGrid.COMPONENT_TYPE)).thenReturn(panelGrid);
		when(mockApplication.createComponent(HtmlPanelGroup.COMPONENT_TYPE)).thenReturn(imageContainer);
		when(mockApplication.createComponent(GraphicImage.COMPONENT_TYPE)).thenReturn(image);

		ContentImage contentImage = new ContentImage();
		contentImage.setBinding("doc.image");
		contentImage.setEditable(Boolean.FALSE);
		contentImage.setShowMarkup(Boolean.FALSE);
		UIComponent result = builder.contentImage(null, "row", contentImage, null, "Image", null);

		assertSame(panelGrid, result);
		assertSame(imageContainer, panelChildren.get(0));
		verify(panelGrid).setColumns(5);
		verify(image).setId("contentImageGrid_doc_image_image");
	}

	@SuppressWarnings("static-method")
	@Test
	void testContentLinkNonShortcutPathWithoutEditableAddsContentLink() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		HtmlPanelGrid panelGrid = mock(HtmlPanelGrid.class);
		HtmlOutputLink outputLink = mock(HtmlOutputLink.class);
		UIOutput outputText = mock(UIOutput.class);
		List<UIComponent> panelChildren = new ArrayList<>();
		List<UIComponent> linkChildren = new ArrayList<>();
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("contentLinkGrid", "contentLinkInner");
		builder.setManagedBeanForTest(managedBean);

		when(panelGrid.getId()).thenReturn("contentLinkGrid");
		when(panelGrid.getChildren()).thenReturn(panelChildren);
		when(outputLink.getChildren()).thenReturn(linkChildren);
		when(mockApplication.createComponent(HtmlPanelGrid.COMPONENT_TYPE)).thenReturn(panelGrid);
		when(mockApplication.createComponent(HtmlOutputLink.COMPONENT_TYPE)).thenReturn(outputLink);
		when(mockApplication.createComponent(UIOutput.COMPONENT_TYPE)).thenReturn(outputText);

		ContentLink contentLink = new ContentLink();
		contentLink.setBinding("doc.attachment");
		contentLink.setEditable(Boolean.FALSE);
		UIComponent result = builder.contentLink(null, "row", contentLink, null, "Attachment", null, HorizontalAlignment.left);

		assertSame(panelGrid, result);
		assertSame(outputLink, panelChildren.get(0));
		assertSame(outputText, linkChildren.get(0));
		verify(panelGrid).setColumns(5);
		verify(outputLink).setId("contentLinkGrid_doc_attachment_link");
	}

	@SuppressWarnings("static-method")
	@Test
	void testHtmlNonShortcutPathConfiguresEditor() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		TextEditor textEditor = mock(TextEditor.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("htmlEditorId");
		builder.setManagedBeanForTest(managedBean);
		when(mockApplication.createComponent(TextEditor.COMPONENT_TYPE)).thenReturn(textEditor);

		HTML html = new HTML();
		html.setBinding("markup");
		html.setSanitise(Sanitisation.none);

		UIComponent result = builder.html(null, "row", html, null, "Markup", null);

		assertSame(textEditor, result);
		verify(textEditor).setSecure(false);
	}

	@SuppressWarnings("static-method")
	@Test
	void testLookupDescriptionDelegatesNonShortcutPath() {
		CapturingInputDelegationBuilder builder = new CapturingInputDelegationBuilder();
		LookupDescription lookup = new LookupDescription();
		lookup.setBinding("customer");
		lookup.setPixelWidth(Integer.valueOf(280));
		lookup.setDisabledConditionName("lookupDisabled");
		QueryDefinition query = mock(QueryDefinition.class);

		EventSourceComponent result = builder.lookupDescription(null,
																	"row",
																	lookup,
																	"formLookupDisabled",
																	"Customer",
																	"Required",
																	HorizontalAlignment.left,
																	"name",
																	query);

		assertSame(builder.delegatedLookupDescriptionResult, result.getComponent());
		assertSame(builder.delegatedLookupDescriptionResult, result.getEventSource());
		assertEquals("row", builder.lookupDataWidgetVar);
		assertEquals("customer", builder.lookupBinding);
		assertEquals("Customer", builder.lookupTitle);
		assertEquals("Required", builder.lookupRequiredMessage);
		assertEquals(HorizontalAlignment.left, builder.lookupTextAlignment);
		assertEquals("lookupDisabled", builder.lookupDisabled);
		assertEquals("formLookupDisabled", builder.lookupFormDisabled);
		assertEquals("name", builder.lookupDisplayBinding);
		assertSame(query, builder.lookupQuery);
		assertEquals(Integer.valueOf(280), builder.lookupPixelWidth);
	}

	@SuppressWarnings("static-method")
	@Test
	void testPasswordDelegatesNonShortcutPathAndSetsRedisplay() {
		CapturingInputDelegationBuilder builder = new CapturingInputDelegationBuilder();
		org.skyve.impl.metadata.view.widget.bound.input.Password password = new org.skyve.impl.metadata.view.widget.bound.input.Password();
		password.setBinding("secret");
		password.setPixelWidth(Integer.valueOf(220));
		password.setDisabledConditionName("passwordDisabled");

		EventSourceComponent result = builder.password(null,
														"row",
														password,
														"formPasswordDisabled",
														"Password",
														null,
														HorizontalAlignment.right);

		assertSame(builder.delegatedPasswordResult, result.getComponent());
		assertSame(builder.delegatedPasswordResult, result.getEventSource());
		assertEquals("row", builder.passwordDataWidgetVar);
		assertEquals("secret", builder.passwordBinding);
		assertEquals("Password", builder.passwordTitle);
		assertEquals(null, builder.passwordRequiredMessage);
		assertEquals(HorizontalAlignment.right, builder.passwordTextAlignment);
		assertEquals("passwordDisabled", builder.passwordDisabled);
		assertEquals("formPasswordDisabled", builder.passwordFormDisabled);
		assertEquals(Integer.valueOf(220), builder.passwordPixelWidth);
		verify(builder.delegatedPasswordResult).setRedisplay(true);
	}

	@SuppressWarnings("static-method")
	@Test
	void testRadioNonShortcutPathAddsSelectItemsAndBindingAttribute() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		SelectOneRadio selectOneRadio = mock(SelectOneRadio.class);
		UISelectItems selectItems = mock(UISelectItems.class);
		List<UIComponent> children = new ArrayList<>();
		Map<String, Object> attributes = new HashMap<>();
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("radioId", "radioItemsId");
		builder.setManagedBeanForTest(managedBean);

		when(selectOneRadio.getChildren()).thenReturn(children);
		when(selectOneRadio.getAttributes()).thenReturn(attributes);
		when(mockApplication.createComponent(SelectOneRadio.COMPONENT_TYPE)).thenReturn(selectOneRadio);
		when(mockApplication.createComponent(UISelectItems.COMPONENT_TYPE)).thenReturn(selectItems);

		Radio radio = new Radio();
		radio.setBinding("status");
		radio.setVertical(Boolean.FALSE);

		EventSourceComponent result = builder.radio(null, "row", radio, null, "Status", null);

		assertSame(selectOneRadio, result.getComponent());
		assertSame(selectOneRadio, result.getEventSource());
		assertSame(selectItems, children.get(0));
		assertEquals("status", attributes.get("binding"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testRichTextNonShortcutPathConfiguresEditor() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		TextEditor textEditor = mock(TextEditor.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("richTextEditorId");
		builder.setManagedBeanForTest(managedBean);
		when(mockApplication.createComponent(TextEditor.COMPONENT_TYPE)).thenReturn(textEditor);

		RichText richText = new RichText();
		richText.setBinding("notes");
		richText.setSanitise(Sanitisation.text);

		EventSourceComponent result = builder.richText(null, "row", richText, null, "Notes", null);

		assertSame(textEditor, result.getComponent());
		assertSame(textEditor, result.getEventSource());
		verify(textEditor).setSecure(true);
		verify(textEditor).setAllowFormatting(false);
		verify(textEditor).setAllowBlocks(false);
		verify(textEditor).setAllowImages(false);
		verify(textEditor).setAllowLinks(false);
		verify(textEditor).setAllowStyles(false);
	}

	@SuppressWarnings("static-method")
	@Test
	void testSpinnerNonShortcutPathConfiguresPrimeSpinner() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		Spinner spinnerComponent = mock(Spinner.class);
		Map<String, Object> passThroughAttributes = new HashMap<>();
		ValueExpression styleClassExpression = mock(ValueExpression.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("spinnerId");
		builder.setManagedBeanForTest(managedBean);

		when(spinnerComponent.getPassThroughAttributes()).thenReturn(passThroughAttributes);
		when(mockApplication.createComponent(Spinner.COMPONENT_TYPE)).thenReturn(spinnerComponent);
		when(mockExpressionFactory.createValueExpression("text-right", String.class)).thenReturn(styleClassExpression);

		org.skyve.impl.metadata.view.widget.bound.input.Spinner spinner = new org.skyve.impl.metadata.view.widget.bound.input.Spinner();
		spinner.setBinding("amount");
		spinner.setKeyboardType(KeyboardType.numeric);
		spinner.setMin(Double.valueOf(1.0));
		spinner.setMax(Double.valueOf(5.0));
		spinner.setStep(Double.valueOf(0.5));
		spinner.setPixelWidth(Integer.valueOf(120));
		spinner.setDisabledConditionName("spinnerDisabled");
		Converter<?> facesConverter = mock(Converter.class);

		EventSourceComponent result = builder.spinner(null,
														"row",
														spinner,
														"formSpinnerDisabled",
														"Amount",
														null,
														HorizontalAlignment.right,
														facesConverter);

		assertSame(spinnerComponent, result.getComponent());
		assertSame(spinnerComponent, result.getEventSource());
		verify(spinnerComponent).setMin(1.0);
		verify(spinnerComponent).setMax(5.0);
		verify(spinnerComponent).setStepFactor(0.5);
		verify(spinnerComponent).setConverter(facesConverter);
		verify(spinnerComponent).setValueExpression("styleClass", styleClassExpression);
		assertEquals("numeric", passThroughAttributes.get("inputmode"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testSliderNonShortcutPathConfiguresHorizontalSlider() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		HtmlPanelGrid panelGrid = mock(HtmlPanelGrid.class);
		HtmlInputHidden hidden = mock(HtmlInputHidden.class);
		HtmlOutputText display = mock(HtmlOutputText.class);
		org.primefaces.component.slider.Slider sliderComponent = mock(org.primefaces.component.slider.Slider.class);
		List<UIComponent> children = new ArrayList<>();
		ValueExpression hiddenValueExpression = mock(ValueExpression.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("sliderPanelId", "sliderHiddenId", "sliderDisplayId", "sliderComponentId");
		builder.setManagedBeanForTest(managedBean);

		when(panelGrid.getChildren()).thenReturn(children);
		when(hidden.getId()).thenReturn("sliderHidden");
		when(hidden.getValueExpression("value")).thenReturn(hiddenValueExpression);
		when(display.getId()).thenReturn("sliderDisplay");
		when(mockApplication.createComponent(HtmlPanelGrid.COMPONENT_TYPE)).thenReturn(panelGrid);
		when(mockApplication.createComponent(HtmlInputHidden.COMPONENT_TYPE)).thenReturn(hidden);
		when(mockApplication.createComponent(HtmlOutputText.COMPONENT_TYPE)).thenReturn(display);
		when(mockApplication.createComponent(org.primefaces.component.slider.Slider.COMPONENT_TYPE)).thenReturn(sliderComponent);

		org.skyve.impl.metadata.view.widget.bound.input.Slider slider = new org.skyve.impl.metadata.view.widget.bound.input.Slider();
		slider.setBinding("percentage");
		slider.setVertical(Boolean.FALSE);
		slider.setMin(Double.valueOf(0.0));
		slider.setMax(Double.valueOf(10.0));
		slider.setNumberOfDiscreteValues(Integer.valueOf(3));
		slider.setRoundingPrecision(Integer.valueOf(2));
		slider.setPixelWidth(Integer.valueOf(240));
		Converter<?> facesConverter = mock(Converter.class);

		EventSourceComponent result = builder.slider(null, "row", slider, null, "Percentage", null, facesConverter);

		assertSame(panelGrid, result.getComponent());
		assertSame(sliderComponent, result.getEventSource());
		verify(panelGrid).setColumns(1);
		verify(panelGrid).setColumnClasses("center");
		verify(panelGrid).setStyle("width:100%");
		verify(sliderComponent).setFor("sliderHidden");
		verify(sliderComponent).setDisplay("sliderDisplay");
		verify(sliderComponent).setDisplayTemplate("{value}");
		verify(sliderComponent).setMinValue(0.0);
		verify(sliderComponent).setMaxValue(10.0);
		verify(sliderComponent).setStep(eq(3.33d));
		verify(sliderComponent).setConverter(facesConverter);
		assertSame(hidden, children.get(0));
		assertSame(sliderComponent, children.get(1));
		assertSame(display, children.get(2));
	}

	@SuppressWarnings("static-method")
	@Test
	void testTextAreaDelegatesNonShortcutPathAndSetsInputMode() {
		CapturingInputDelegationBuilder builder = new CapturingInputDelegationBuilder();
		Map<String, Object> passThroughAttributes = new HashMap<>();
		when(builder.delegatedTextAreaResult.getPassThroughAttributes()).thenReturn(passThroughAttributes);

		TextArea textArea = new TextArea();
		textArea.setBinding("notes");
		textArea.setEditable(Boolean.FALSE);
		textArea.setKeyboardType(KeyboardType.tel);
		textArea.setDisabledConditionName("textAreaDisabled");
		textArea.setPixelWidth(Integer.valueOf(300));
		textArea.setPixelHeight(Integer.valueOf(80));

		EventSourceComponent result = builder.textArea(null,
														"row",
														textArea,
														"formTextAreaDisabled",
														"Notes",
														"Required",
														HorizontalAlignment.left,
														Integer.valueOf(255));

		assertSame(builder.delegatedTextAreaResult, result.getComponent());
		assertSame(builder.delegatedTextAreaResult, result.getEventSource());
		assertEquals("row", builder.textAreaDataWidgetVar);
		assertEquals("notes", builder.textAreaBinding);
		assertEquals("Notes", builder.textAreaTitle);
		assertEquals("Required", builder.textAreaRequiredMessage);
		assertEquals(HorizontalAlignment.left, builder.textAreaTextAlignment);
		assertEquals(true, builder.textAreaReadonly);
		assertEquals("textAreaDisabled", builder.textAreaDisabled);
		assertEquals("formTextAreaDisabled", builder.textAreaFormDisabled);
		assertEquals(Integer.valueOf(255), builder.textAreaMaxLength);
		assertEquals(Integer.valueOf(300), builder.textAreaPixelWidth);
		assertEquals(Integer.valueOf(80), builder.textAreaPixelHeight);
		assertEquals("tel", passThroughAttributes.get("inputmode"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testTextDelegatesToTextFieldWhenNoConverterOrComplete() {
		CapturingInputDelegationBuilder builder = new CapturingInputDelegationBuilder();
		TextField textField = new TextField();
		textField.setBinding("name");
		textField.setDisabledConditionName("textDisabled");
		textField.setKeyboardType(KeyboardType.search);
		textField.setPixelWidth(Integer.valueOf(180));
		Converter<?> facesConverter = mock(Converter.class);

		EventSourceComponent result = builder.text(null,
													"row",
													textField,
													"formTextDisabled",
													"Name",
													"Required",
													HorizontalAlignment.left,
													Integer.valueOf(40),
													null,
													null,
													facesConverter);

		assertSame(builder.delegatedTextFieldResult, result.getComponent());
		assertSame(builder.delegatedTextFieldResult, result.getEventSource());
		assertEquals("row", builder.textFieldDataWidgetVar);
		assertEquals("name", builder.textFieldBinding);
		assertEquals("Name", builder.textFieldTitle);
		assertEquals("Required", builder.textFieldRequiredMessage);
		assertEquals(HorizontalAlignment.left, builder.textFieldTextAlignment);
		assertEquals(false, builder.textFieldReadonly);
		assertEquals("textDisabled", builder.textFieldDisabled);
		assertEquals("formTextDisabled", builder.textFieldFormDisabled);
		assertEquals(Integer.valueOf(40), builder.textFieldMaxLength);
		assertSame(facesConverter, builder.textFieldConverter);
		assertEquals(KeyboardType.search, builder.textFieldKeyboardType);
		assertEquals(Integer.valueOf(180), builder.textFieldPixelWidth);
	}

	@SuppressWarnings("static-method")
	@Test
	void testTextDelegatesToCompleteWhenCompleteConfigured() {
		CapturingInputDelegationBuilder builder = new CapturingInputDelegationBuilder();
		TextField textField = new TextField();
		textField.setBinding("suburb");
		textField.setComplete(CompleteType.suggest);
		textField.setKeyboardType(KeyboardType.search);
		textField.setPixelWidth(Integer.valueOf(220));
		textField.setDisabledConditionName("completeDisabled");

		EventSourceComponent result = builder.text(null,
													"row",
													textField,
													"formCompleteDisabled",
													"Suburb",
													null,
													HorizontalAlignment.right,
													Integer.valueOf(80),
													null,
													null,
													null);

		assertSame(builder.delegatedCompleteResult, result.getComponent());
		assertSame(builder.delegatedCompleteResult, result.getEventSource());
		assertEquals("row", builder.completeDataWidgetVar);
		assertEquals("suburb", builder.completeBinding);
		assertEquals("Suburb", builder.completeTitle);
		assertEquals(HorizontalAlignment.right, builder.completeTextAlignment);
		assertEquals("completeDisabled", builder.completeDisabled);
		assertEquals(Integer.valueOf(80), builder.completeLength);
		assertEquals("formCompleteDisabled", builder.completeFormDisabled);
		assertEquals(CompleteType.suggest, builder.completeType);
		assertEquals(KeyboardType.search, builder.completeKeyboardType);
		assertEquals(Integer.valueOf(220), builder.completePixelWidth);
	}

	@SuppressWarnings("static-method")
	@Test
	void testActionLinkDelegatesToActionLinkWhenNotDownload() {
		CapturingDelegationBuilder builder = new CapturingDelegationBuilder();
		Link link = new Link();
		link.setPixelWidth(Integer.valueOf(130));
		link.getProperties().put("process", "@this");
		link.getProperties().put("update", "@none");

		Action action = mock(Action.class);
		when(action.getImplicitName()).thenReturn(ImplicitActionName.Save);
		when(action.getName()).thenReturn("saveAction");
		when(action.getClientValidation()).thenReturn(Boolean.TRUE);
		when(action.getDisabledConditionName()).thenReturn("linkDisabled");
		when(action.getInvisibleConditionName()).thenReturn("linkInvisible");

		UIComponent result = builder.actionLink(null,
													"orders",
													"row",
													"Save",
													null,
													"Save row",
													"Confirm save?",
													link,
													action);

		assertSame(builder.delegatedActionLinkResult, result);
		assertEquals("Save", builder.linkTitle);
		assertEquals("Save row", builder.linkTooltip);
		assertEquals(null, builder.linkImplicitName);
		assertEquals("saveAction", builder.linkActionName);
		assertEquals(false, builder.linkInline);
		assertEquals("orders", builder.linkDataWidgetBinding);
		assertEquals("row", builder.linkDataWidgetVar);
		assertEquals(Integer.valueOf(130), builder.linkPixelWidth);
		assertEquals(Boolean.TRUE, builder.linkClientValidation);
		assertEquals("Confirm save?", builder.linkConfirmationText);
		assertEquals("linkDisabled", builder.linkDisabled);
		assertEquals("linkInvisible", builder.linkInvisible);
		assertEquals("@this", builder.linkProcessOverride);
		assertEquals("@none", builder.linkUpdateOverride);
	}

	@SuppressWarnings("static-method")
	@Test
	void testActionLinkDelegatesToDownloadLinkWhenImplicitDownload() {
		CapturingDelegationBuilder builder = new CapturingDelegationBuilder();
		Link link = new Link();
		link.setPixelWidth(Integer.valueOf(95));
		link.getProperties().put("process", "@form");
		link.getProperties().put("update", "@(form)");

		Action action = mock(Action.class);
		when(action.getImplicitName()).thenReturn(ImplicitActionName.Download);
		when(action.getName()).thenReturn("downloadCsv");
		when(action.getDisabledConditionName()).thenReturn("downloadDisabled");
		when(action.getInvisibleConditionName()).thenReturn("downloadInvisible");

		UIComponent result = builder.actionLink(null,
													"orders",
													"row",
													"Download",
													null,
													"Download row",
													null,
													link,
													action);

		assertSame(builder.delegatedDownloadLinkResult, result);
		assertEquals("Download", builder.downloadLinkTitle);
		assertEquals("Download row", builder.downloadLinkTooltip);
		assertEquals("downloadCsv", builder.downloadLinkActionName);
		assertEquals("orders", builder.downloadLinkDataWidgetBinding);
		assertEquals("row", builder.downloadLinkDataWidgetVar);
		assertEquals(Integer.valueOf(95), builder.downloadLinkPixelWidth);
		assertEquals("downloadDisabled", builder.downloadLinkDisabled);
		assertEquals("downloadInvisible", builder.downloadLinkInvisible);
		assertEquals("@form", builder.downloadLinkProcessOverride);
		assertEquals("@(form)", builder.downloadLinkUpdateOverride);
	}

	@SuppressWarnings("static-method")
	@Test
	void testDownloadNonShortcutPathUsesOverridesFromActionProperties() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		CommandButton commandButton = mock(CommandButton.class);
		MethodExpression actionExpression = mock(MethodExpression.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("downloadLinkButtonId");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(CommandButton.COMPONENT_TYPE)).thenReturn(commandButton);
		when(mockExpressionFactory.createMethodExpression(any(ELContext.class), anyString(), isNull(), any(Class[].class))).thenReturn(actionExpression);

		Action action = mock(Action.class);
		when(action.getName()).thenReturn("downloadInvoice");
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		Map<String, String> properties = new HashMap<>();
		properties.put("process", "@this");
		properties.put("update", "@none");
		when(action.getProperties()).thenReturn(properties);

		UIComponent result = builder.download(null,
												"orders",
												"row",
												"Download",
												"pi pi-download",
												"Download invoice",
												null,
												action);

		assertSame(commandButton, result);
		verify(commandButton).setProcess("@this");
		verify(commandButton).setUpdate("@none");
		verify(commandButton).setActionExpression(actionExpression);
	}

	@SuppressWarnings("static-method")
	@Test
	void testUploadDelegatesNonShortcutPath() {
		CapturingDelegationBuilder builder = new CapturingDelegationBuilder();
		builder.delegatedUploadButtonResult = new HtmlPanelGroup();

		Action action = mock(Action.class);
		when(action.getName()).thenReturn("uploadContent");
		when(action.getClientValidation()).thenReturn(Boolean.TRUE);
		when(action.getDisabledConditionName()).thenReturn("uploadDisabled");
		when(action.getInvisibleConditionName()).thenReturn("uploadInvisible");

		UIComponent result = builder.upload(null, "Upload", "pi pi-upload", "Upload file", "Proceed?", action);

		assertSame(builder.delegatedUploadButtonResult, result);
		assertEquals("Upload", builder.uploadTitle);
		assertEquals("pi pi-upload", builder.uploadIconStyleClass);
		assertEquals("Upload file", builder.uploadTooltip);
		assertEquals("uploadContent", builder.uploadActionName);
		assertEquals(null, builder.uploadPixelWidth);
		assertEquals(null, builder.uploadPixelHeight);
		assertEquals(Boolean.TRUE, builder.uploadClientValidation);
		assertEquals("Proceed?", builder.uploadConfirmationText);
		assertEquals("uploadDisabled", builder.uploadDisabled);
		assertEquals(null, builder.uploadFormDisabled);
		assertEquals("uploadInvisible", builder.uploadInvisible);
		assertEquals(false, builder.uploadUseDialog);
	}

	@SuppressWarnings("static-method")
	@Test
	void testRemoveDelegatesNonShortcutPath() {
		CapturingDelegationBuilder builder = new CapturingDelegationBuilder();
		builder.delegatedActionButtonResult = mock(CommandButton.class);

		Action action = mock(Action.class);
		when(action.getName()).thenReturn("removeLine");
		when(action.getDisabledConditionName()).thenReturn("removeDisabled");
		when(action.getInvisibleConditionName()).thenReturn("removeInvisible");
		Map<String, String> properties = new HashMap<>();
		properties.put("process", "@this");
		properties.put("update", "@none");
		when(action.getProperties()).thenReturn(properties);

		UIComponent result = builder.remove(null, "Remove", "pi pi-times", "Remove row", "Confirm remove?", action, true);

		assertSame(builder.delegatedActionButtonResult, result);
		assertEquals("Remove", builder.actionTitle);
		assertEquals("pi pi-times", builder.actionIconStyleClass);
		assertEquals("Remove row", builder.actionTooltip);
		assertEquals(ImplicitActionName.Remove, builder.actionImplicitName);
		assertEquals("removeLine", builder.actionName);
		assertEquals(false, builder.actionInline);
		assertEquals(null, builder.actionDataWidgetBinding);
		assertEquals(null, builder.actionDataWidgetVar);
		assertEquals(null, builder.actionPixelWidth);
		assertEquals(null, builder.actionPixelHeight);
		assertEquals("Confirm remove?", builder.actionConfirmationText);
		assertEquals("removeDisabled", builder.actionDisabled);
		assertEquals(null, builder.actionFormDisabled);
		assertEquals("removeInvisible", builder.actionInvisible);
		assertEquals("@this", builder.actionProcessOverride);
		assertEquals("@none", builder.actionUpdateOverride);
		assertEquals(true, builder.actionCanDelete);
	}

	@SuppressWarnings("static-method")
	@Test
	void testActionDelegatesNonShortcutPath() {
		CapturingDelegationBuilder builder = new CapturingDelegationBuilder();
		builder.delegatedActionButtonResult = mock(CommandButton.class);

		Action action = mock(Action.class);
		when(action.getName()).thenReturn("approve");
		when(action.getDisabledConditionName()).thenReturn("approveDisabled");
		when(action.getInvisibleConditionName()).thenReturn("approveInvisible");
		Map<String, String> properties = new HashMap<>();
		properties.put("process", "@form");
		properties.put("update", "@(form)");
		when(action.getProperties()).thenReturn(properties);

		UIComponent result = builder.action(null,
												"orders",
												"row",
												"Approve",
												"pi pi-check",
												"Approve row",
												"Confirm approve?",
												ImplicitActionName.OK,
												action);

		assertSame(builder.delegatedActionButtonResult, result);
		assertEquals("Approve", builder.actionTitle);
		assertEquals("pi pi-check", builder.actionIconStyleClass);
		assertEquals("Approve row", builder.actionTooltip);
		assertEquals(ImplicitActionName.OK, builder.actionImplicitName);
		assertEquals("approve", builder.actionName);
		assertEquals(false, builder.actionInline);
		assertEquals("orders", builder.actionDataWidgetBinding);
		assertEquals("row", builder.actionDataWidgetVar);
		assertEquals(null, builder.actionPixelWidth);
		assertEquals(null, builder.actionPixelHeight);
		assertEquals("Confirm approve?", builder.actionConfirmationText);
		assertEquals("approveDisabled", builder.actionDisabled);
		assertEquals(null, builder.actionFormDisabled);
		assertEquals("approveInvisible", builder.actionInvisible);
		assertEquals("@form", builder.actionProcessOverride);
		assertEquals("@(form)", builder.actionUpdateOverride);
		assertEquals(false, builder.actionCanDelete);
	}

	@SuppressWarnings({ "static-method", "unchecked" })
	@Test
	void testReportNonShortcutPathBuildsReportHref() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		org.primefaces.component.button.Button reportButton = mock(org.primefaces.component.button.Button.class);
		ValueExpression valueExpression = mock(ValueExpression.class);
		AtomicReference<String> capturedHref = new AtomicReference<>();
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("reportButtonId");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(org.primefaces.component.button.Button.COMPONENT_TYPE)).thenReturn(reportButton);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(String.class))).thenAnswer(invocation -> {
			String expression = invocation.getArgument(1, String.class);
			if (expression.startsWith("report/")) {
				capturedHref.set(expression);
			}
			return valueExpression;
		});

		org.skyve.metadata.view.widget.bound.Parameter reportNameParam = mock(org.skyve.metadata.view.widget.bound.Parameter.class);
		when(reportNameParam.getName()).thenReturn(AbstractWebContext.REPORT_NAME);
		when(reportNameParam.getValue()).thenReturn("Orders");
		when(reportNameParam.getValueBinding()).thenReturn(null);
		List<org.skyve.metadata.view.widget.bound.Parameter> parameters = new ArrayList<>();
		parameters.add(reportNameParam);

		Action action = mock(Action.class);
		when(action.getParameters()).thenReturn(parameters);
		when(action.getClientValidation()).thenReturn(Boolean.FALSE);
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);

		UIComponent result = builder.report(null, "Run Report", null, "Open report", null, action);

		assertSame(reportButton, result);
		assertNotNull(capturedHref.get());
		assertTrue(capturedHref.get().contains("report/Orders.pdf?"));
		verify(reportButton).setValue("Run Report");
		verify(reportButton).setTitle("Open report");
	}

	@SuppressWarnings("static-method")
	@Test
	void testListRepeaterNonShortcutPathConfiguresDataTable() {
		CapturingListBuilder builder = new CapturingListBuilder();
		DataTable dataTable = mock(DataTable.class);
		List<UIComponent> children = new ArrayList<>();
		ValueExpression modelExpression = mock(ValueExpression.class);
		AtomicReference<String> capturedModelExpression = new AtomicReference<>();
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("listRepeaterId");
		builder.setManagedBeanForTest(managedBean);

		when(dataTable.getChildren()).thenReturn(children);
		when(dataTable.getId()).thenReturn("listRepeaterId");
		when(dataTable.getWidgetVar()).thenReturn("listRepeaterId");
		when(mockApplication.createComponent(DataTable.COMPONENT_TYPE)).thenReturn(dataTable);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(SkyveLazyDataModel.class))).thenAnswer(invocation -> {
			capturedModelExpression.set(invocation.getArgument(1, String.class));
			return modelExpression;
		});

		@SuppressWarnings("unchecked")
		ListModel<org.skyve.domain.Bean> model = mock(ListModel.class);
		Document drivingDocument = mock(Document.class);
		when(model.getDrivingDocument()).thenReturn(drivingDocument);
		when(drivingDocument.getOwningModuleName()).thenReturn("sales");
		when(drivingDocument.getName()).thenReturn("Order");

		UIComponent result = builder.listRepeater(null,
													"Order",
													"recentOrders",
													"desktop",
													model,
													null,
													null,
													false,
													false);

		assertSame(dataTable, result);
		assertSame(model, builder.capturedModel);
		assertSame(children, builder.capturedChildren);
		assertEquals(false, builder.capturedShowFilter);
		assertEquals("listRepeaterId", builder.capturedWidgetVar);
		assertEquals("desktop", builder.capturedUxui);
		assertNotNull(capturedModelExpression.get());
		assertTrue(capturedModelExpression.get().contains("getLazyDataModel('sales','Order',null,'recentOrders',null)"));
		verify(dataTable).setStyleClass("repeater repeater-no-headers repeater-no-border");
		verify(dataTable).setScrollable(true);
		verify(dataTable).setScrollRows(50);
		verify(dataTable).setLiveScroll(true);
		verify(dataTable).setValueExpression("value", modelExpression);
	}

	@SuppressWarnings("static-method")
	@Test
	void testListGridNonShortcutPathConfiguresDataTableAndDelegatesColumnsAndActionColumn() {
		CapturingListGridBuilder builder = new CapturingListGridBuilder();
		DataTable dataTable = mock(DataTable.class);
		UIOutput emptyMessage = mock(UIOutput.class);
		List<UIComponent> children = new ArrayList<>();
		Map<String, UIComponent> facets = new HashMap<>();
		ValueExpression modelExpression = mock(ValueExpression.class);
		FacesView managedBean = mock(FacesView.class);
		org.skyve.domain.Bean bean = mock(org.skyve.domain.Bean.class);
		org.skyve.impl.web.faces.models.BeanMapAdapter currentBean = new org.skyve.impl.web.faces.models.BeanMapAdapter(bean, null);

		when(managedBean.nextId()).thenReturn("listGridId");
		when(managedBean.getCurrentBean()).thenReturn(currentBean);
		builder.setManagedBeanForTest(managedBean);

		when(dataTable.getChildren()).thenReturn(children);
		when(dataTable.getFacets()).thenReturn(facets);
		when(dataTable.getId()).thenReturn("listGridId");
		when(dataTable.getWidgetVar()).thenReturn("listGridId");
		when(mockApplication.createComponent(DataTable.COMPONENT_TYPE)).thenReturn(dataTable);
		when(mockApplication.createComponent(UIOutput.COMPONENT_TYPE)).thenReturn(emptyMessage);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(SkyveLazyDataModel.class))).thenReturn(modelExpression);

		@SuppressWarnings("unchecked")
		ListModel<org.skyve.domain.Bean> model = mock(ListModel.class);
		Document drivingDocument = mock(Document.class);
		when(model.getDrivingDocument()).thenReturn(drivingDocument);
		when(drivingDocument.getOwningModuleName()).thenReturn("sales");
		when(drivingDocument.getName()).thenReturn("Order");

		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		AbstractPersistence previousPersistence = currentPersistenceIfPresent();
		when(user.canCreateDocument(drivingDocument)).thenReturn(true);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(persistence.getUser()).thenReturn(user);

		ListGrid grid = new ListGrid();
		grid.setShowZoom(Boolean.FALSE);
		grid.setShowFilter(Boolean.TRUE);
		grid.setDisableAddConditionName("disableAdd");
		grid.setDisabledConditionName("disableGrid");
		grid.getProperties().put("process", "@this");

		UIComponent result;
		try {
			persistence.setForThread();
			result = builder.listGrid(null, "sales", "Order", "recentOrders", "desktop", model, null, grid, false);
		}
		finally {
			restorePersistence(previousPersistence);
		}

		assertSame(dataTable, result);
		assertSame(emptyMessage, facets.get("emptyMessage"));
		assertSame(model, builder.capturedModel);
		assertSame(children, builder.capturedChildren);
		assertEquals(true, builder.capturedShowFilter);
		assertEquals("listGridId", builder.capturedWidgetVar);
		assertEquals("desktop", builder.capturedUxui);
		assertEquals(1, children.size());
		assertSame(builder.actionColumnToReturn, children.get(0));
		assertEquals(true, builder.actionColumnCalled);
		assertEquals("sales", builder.actionModuleName);
		assertEquals("Order", builder.actionDocumentName);
		assertEquals(true, builder.actionCanCreateDocument);
		assertEquals(true, builder.actionCreateRendered);
		assertEquals(2, builder.actionCreateDisabled.length);
		assertEquals("disableAdd", builder.actionCreateDisabled[0]);
		assertEquals("disableGrid", builder.actionCreateDisabled[1]);
		assertEquals(null, builder.actionCreateUrlParams);
		assertEquals(false, builder.actionZoomRendered);
		assertEquals(null, builder.actionZoomDisabledConditionName);
		assertEquals(true, builder.actionShowFilter);
		assertEquals("listGridId", builder.actionParentId);
		assertSame(grid.getProperties(), builder.actionProperties);
		verify(model).setBean(bean);
		verify(dataTable).setVar("row");
		verify(dataTable).setLazy(true);
		verify(dataTable).setRows(50);
		verify(dataTable).setSortMode("multiple");
		verify(dataTable).setFilterDelay(500);
		verify(dataTable).setValueExpression("value", modelExpression);
		verify(emptyMessage).setValue(TabularComponentBuilder.EMPTY_DATA_TABLE_CAN_ADD_MESSAGE);
	}

	@SuppressWarnings("static-method")
	@Test
	void testListMembershipNonShortcutPathSetsDefaultFacetHeadings() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		PickList pickList = mock(PickList.class);
		UIOutput sourceCaption = mock(UIOutput.class);
		UIOutput targetCaption = mock(UIOutput.class);
		Map<String, UIComponent> facets = new HashMap<>();
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("pickListId", "sourceCaptionId", "targetCaptionId");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(PickList.COMPONENT_TYPE)).thenReturn(pickList);
		when(mockApplication.createComponent(UIOutput.COMPONENT_TYPE)).thenReturn(sourceCaption, targetCaption);
		when(pickList.getFacets()).thenReturn(facets);

		org.skyve.impl.metadata.view.widget.bound.input.ListMembership membership = new org.skyve.impl.metadata.view.widget.bound.input.ListMembership();
		membership.setBinding("roles");

		EventSourceComponent result = builder.listMembership(null, null, null, membership);

		assertSame(pickList, result.getComponent());
		assertSame(pickList, result.getEventSource());
		assertSame(sourceCaption, facets.get("sourceCaption"));
		assertSame(targetCaption, facets.get("targetCaption"));
		verify(sourceCaption).setValue("Candidates");
		verify(targetCaption).setValue("Members");
	}

	@SuppressWarnings("static-method")
	@Test
	void testCreateListGridZoomButtonAppliesDisabledConditionAndOnclick() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		CommandButton button = mock(CommandButton.class);
		ValueExpression disabledExpression = mock(ValueExpression.class);
		ValueExpression onclickExpression = mock(ValueExpression.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("zoomButtonId");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(CommandButton.COMPONENT_TYPE)).thenReturn(button);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(Boolean.class))).thenReturn(disabledExpression);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(String.class))).thenReturn(onclickExpression);

		UIComponent result = builder.createListGridZoomButton("disableZoom", Map.of());

		assertSame(button, result);
		verify(button).setTitle("View Detail");
		verify(button).setType("button");
		verify(button).setValueExpression("disabled", disabledExpression);
		verify(button).setValueExpression("onclick", onclickExpression);
	}

	@SuppressWarnings("static-method")
	@Test
	void testListGridContextMenuAddsZoomItemsWithDisabledExpression() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		ContextMenu contextMenu = mock(ContextMenu.class);
		UIMenuItem zoomItem = mock(UIMenuItem.class);
		UIMenuItem popoutItem = mock(UIMenuItem.class);
		List<UIComponent> items = new ArrayList<>();
		ValueExpression disabledExpression = mock(ValueExpression.class);

		when(mockApplication.createComponent(ContextMenu.COMPONENT_TYPE)).thenReturn(contextMenu);
		when(mockApplication.createComponent(UIMenuItem.COMPONENT_TYPE)).thenReturn(zoomItem, popoutItem);
		when(contextMenu.getChildren()).thenReturn(items);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(Boolean.class))).thenReturn(disabledExpression);

		ListGrid grid = new ListGrid();
		grid.setDisableZoomConditionName("disableZoom");

		UIComponent result = builder.listGridContextMenu(null, "listGrid1", grid);

		assertSame(contextMenu, result);
		assertEquals(2, items.size());
		assertSame(zoomItem, items.get(0));
		assertSame(popoutItem, items.get(1));
		verify(contextMenu).setFor("listGrid1");
		verify(zoomItem).setValueExpression("disabled", disabledExpression);
		verify(popoutItem).setValueExpression("disabled", disabledExpression);
	}

	@SuppressWarnings("static-method")
	@Test
	void testAddContentSignatureNonShortcutPathBuildsSignatureAndButtons() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("signatureId", "imagePanelId", "imageId", "buttonGridId", "signButtonId", "clearClientId", "clearServerId");
		builder.setManagedBeanForTest(managedBean);

		UIComponent layout = mock(UIComponent.class);
		List<UIComponent> layoutChildren = new ArrayList<>();
		when(layout.getId()).thenReturn("sigLayout");
		when(layout.getClientId()).thenReturn("form:sigLayout");
		when(layout.getChildren()).thenReturn(layoutChildren);

		Signature signatureComponent = mock(Signature.class);
		HtmlPanelGroup imagePanel = mock(HtmlPanelGroup.class);
		List<UIComponent> imageChildren = new ArrayList<>();
		when(imagePanel.getChildren()).thenReturn(imageChildren);
		GraphicImage image = mock(GraphicImage.class);
		HtmlPanelGrid buttonGrid = mock(HtmlPanelGrid.class);
		List<UIComponent> buttonChildren = new ArrayList<>();
		when(buttonGrid.getChildren()).thenReturn(buttonChildren);
		CommandButton signButton = mock(CommandButton.class);
		CommandButton clearClientButton = mock(CommandButton.class);
		CommandButton clearServerButton = mock(CommandButton.class);
		ValueExpression valueExpression = mock(ValueExpression.class);
		MethodExpression methodExpression = mock(MethodExpression.class);

		when(mockApplication.createComponent(Signature.COMPONENT_TYPE)).thenReturn(signatureComponent);
		when(mockApplication.createComponent(HtmlPanelGroup.COMPONENT_TYPE)).thenReturn(imagePanel);
		when(mockApplication.createComponent(GraphicImage.COMPONENT_TYPE)).thenReturn(image);
		when(mockApplication.createComponent(HtmlPanelGrid.COMPONENT_TYPE)).thenReturn(buttonGrid);
		when(mockApplication.createComponent(CommandButton.COMPONENT_TYPE)).thenReturn(signButton, clearClientButton, clearServerButton);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(String.class))).thenReturn(valueExpression);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(Boolean.class))).thenReturn(valueExpression);
		when(mockExpressionFactory.createMethodExpression(any(ELContext.class), anyString(), isNull(), any(Class[].class))).thenReturn(methodExpression);

		ContentSignature signature = new ContentSignature();
		signature.setBinding("customerSignature");

		UIComponent result = builder.addContentSignature(null, layout, signature, null, "Signature", null);

		assertSame(signatureComponent, result);
		assertEquals(3, layoutChildren.size());
		assertSame(signatureComponent, layoutChildren.get(0));
		assertSame(imagePanel, layoutChildren.get(1));
		assertSame(buttonGrid, layoutChildren.get(2));
		assertSame(image, imageChildren.get(0));
		assertEquals(3, buttonChildren.size());
		verify(signatureComponent).setStyle("width:400px;height:200px");
		verify(signButton).setValue("Sign");
		verify(signButton).setProcess("@this");
		verify(signButton).setUpdate("sigLayout");
	}

	@SuppressWarnings("static-method")
	@Test
	void testCreateListGridActionColumnAddsFilterCreateAndZoomControls() {
		ListGridActionColumnBuilder builder = new ListGridActionColumnBuilder(mock(UIComponent.class), mock(UIComponent.class));

		Column actionColumn = mock(Column.class);
		HtmlPanelGroup columnHeader = mock(HtmlPanelGroup.class);
		CommandButton createButton = mock(CommandButton.class);
		ValueExpression boolExpression = mock(ValueExpression.class);
		ValueExpression stringExpression = mock(ValueExpression.class);
		Map<String, UIComponent> facets = new HashMap<>();
		List<UIComponent> headerChildren = new ArrayList<>();
		List<UIComponent> actionChildren = new ArrayList<>();
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("createButtonId");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(Column.COMPONENT_TYPE)).thenReturn(actionColumn);
		when(mockApplication.createComponent(HtmlPanelGroup.COMPONENT_TYPE)).thenReturn(columnHeader);
		when(mockApplication.createComponent(CommandButton.COMPONENT_TYPE)).thenReturn(createButton);
		when(actionColumn.getFacets()).thenReturn(facets);
		when(actionColumn.getChildren()).thenReturn(actionChildren);
		when(columnHeader.getChildren()).thenReturn(headerChildren);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(Boolean.class))).thenReturn(boolExpression);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(String.class))).thenReturn(stringExpression);

		UIComponent result = builder.createListGridActionColumn("sales",
																"Order",
																true,
																true,
																new String[] {"canCreate"},
																"&from=quick",
																true,
																"zoomDisabled",
																true,
																"listGrid1",
																Map.of());

		assertSame(actionColumn, result);
		assertEquals(2, headerChildren.size());
		assertEquals(1, actionChildren.size());
		verify(actionColumn).setWidth("60");
		verify(actionColumn).setStyle("text-align:center !important");
		verify(createButton).setValueExpression("disabled", boolExpression);
		verify(createButton).setValueExpression("onclick", stringExpression);
	}

	@SuppressWarnings("static-method")
	@Test
	void testCreateListGridActionColumnSetsBlankHeaderWhenCreateNotRendered() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		Column actionColumn = mock(Column.class);
		HtmlPanelGroup columnHeader = mock(HtmlPanelGroup.class);
		Map<String, UIComponent> facets = new HashMap<>();
		List<UIComponent> headerChildren = new ArrayList<>();
		List<UIComponent> actionChildren = new ArrayList<>();

		when(mockApplication.createComponent(Column.COMPONENT_TYPE)).thenReturn(actionColumn);
		when(mockApplication.createComponent(HtmlPanelGroup.COMPONENT_TYPE)).thenReturn(columnHeader);
		when(actionColumn.getFacets()).thenReturn(facets);
		when(actionColumn.getChildren()).thenReturn(actionChildren);
		when(columnHeader.getChildren()).thenReturn(headerChildren);

		UIComponent result = builder.createListGridActionColumn("sales",
																"Order",
																false,
																false,
																null,
																null,
																false,
																null,
																false,
																"listGrid1",
																Map.of());

		assertSame(actionColumn, result);
		assertEquals(0, headerChildren.size());
		assertEquals(0, actionChildren.size());
		verify(actionColumn).setHeaderText("");
	}

	@SuppressWarnings("static-method")
	@Test
	void testAddListGridDataColumnsAddsProjectedColumnWithoutMetadataBindingLookup() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		Column column = mock(Column.class);
		HtmlOutputText outputText = mock(HtmlOutputText.class);
		List<UIComponent> listChildren = new ArrayList<>();
		List<UIComponent> columnChildren = new ArrayList<>();
		ValueExpression valueExpression = mock(ValueExpression.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("listColumnId");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(Column.COMPONENT_TYPE)).thenReturn(column);
		when(mockApplication.createComponent(HtmlOutputText.COMPONENT_TYPE)).thenReturn(outputText);
		when(column.getChildren()).thenReturn(columnChildren);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(Object.class))).thenReturn(valueExpression);

		@SuppressWarnings("unchecked")
		ListModel<org.skyve.domain.Bean> model = mock(ListModel.class);
		Document drivingDocument = mock(Document.class);
		when(model.getDrivingDocument()).thenReturn(drivingDocument);
		when(drivingDocument.getOwningModuleName()).thenReturn("sales");

		MetaDataQueryProjectedColumn queryColumn = mock(MetaDataQueryProjectedColumn.class);
		when(model.getColumns()).thenReturn(List.of(queryColumn));
		when(queryColumn.isHidden()).thenReturn(false);
		when(queryColumn.isProjected()).thenReturn(true);
		when(queryColumn.getName()).thenReturn("status");
		when(queryColumn.getBinding()).thenReturn(null);
		when(model.determineColumnTitle(queryColumn)).thenReturn("Status");
		when(queryColumn.getPixelWidth()).thenReturn(Integer.valueOf(120));
		when(queryColumn.getAlignment()).thenReturn(HorizontalAlignment.left);
		when(queryColumn.getFormatterName()).thenReturn(null);
		when(queryColumn.getCustomFormatterName()).thenReturn(null);

		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		Customisations customisations = mock(Customisations.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		AbstractPersistence previousPersistence = currentPersistenceIfPresent();
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(persistence.getUser()).thenReturn(user);
		Customisations previousCustomisations = CustomisationsStaticSingleton.get();
		try {
			CustomisationsStaticSingleton.set(customisations);
			persistence.setForThread();
			builder.addListGridDataColumns(model, listChildren, false, "listTable", "desktop");
		}
		finally {
			CustomisationsStaticSingleton.set(previousCustomisations);
			restorePersistence(previousPersistence);
		}

		assertEquals(1, listChildren.size());
		assertSame(column, listChildren.get(0));
		assertEquals(1, columnChildren.size());
		assertSame(outputText, columnChildren.get(0));
		verify(column).setHeaderText("Status");
		verify(column).setField("status");
		verify(column).setSortable(false);
		verify(column).setFilterable(false);
		verify(column).setStyle("width:120px;text-align:left !important;");
		verify(outputText).setValueExpression("value", valueExpression);
		verify(outputText).setEscape(false);
	}

	@SuppressWarnings({ "static-method", "boxing" })
	@Test
	void testActionButtonDelegatesNonShortcutPath() {
		CapturingDelegationBuilder builder = new CapturingDelegationBuilder();
		builder.delegatedActionButtonResult = mock(CommandButton.class);

		Button button = new Button();
		button.setPixelWidth(Integer.valueOf(120));
		button.setPixelHeight(Integer.valueOf(40));
		button.getProperties().put("process", "@this");
		button.getProperties().put("update", "@none");

		Action action = mock(Action.class);
		when(action.getImplicitName()).thenReturn(ImplicitActionName.OK);
		when(action.getName()).thenReturn("saveAction");
		when(action.getDisabledConditionName()).thenReturn("disabledExpr");
		when(action.getInvisibleConditionName()).thenReturn("invisibleExpr");

		UIComponent result = builder.actionButton(null,
													"orders",
													"row",
													"Save",
													"pi pi-check",
													"Save row",
													"Confirm save?",
													button,
													"formDisabledExpr",
													action);

		assertSame(builder.delegatedActionButtonResult, result);
		assertEquals("Save", builder.actionTitle);
		assertEquals("pi pi-check", builder.actionIconStyleClass);
		assertEquals("Save row", builder.actionTooltip);
		assertEquals(ImplicitActionName.OK, builder.actionImplicitName);
		assertEquals("saveAction", builder.actionName);
		assertEquals(false, builder.actionInline);
		assertEquals("orders", builder.actionDataWidgetBinding);
		assertEquals("row", builder.actionDataWidgetVar);
		assertEquals(Integer.valueOf(120), builder.actionPixelWidth);
		assertEquals(Integer.valueOf(40), builder.actionPixelHeight);
		assertEquals("Confirm save?", builder.actionConfirmationText);
		assertEquals("disabledExpr", builder.actionDisabled);
		assertEquals("formDisabledExpr", builder.actionFormDisabled);
		assertEquals("invisibleExpr", builder.actionInvisible);
		assertEquals("@this", builder.actionProcessOverride);
		assertEquals("@none", builder.actionUpdateOverride);
		assertEquals(false, builder.actionCanDelete);
	}

	@SuppressWarnings({ "static-method", "boxing" })
	@Test
	void testUploadButtonDelegatesNonShortcutPath() {
		CapturingDelegationBuilder builder = new CapturingDelegationBuilder();
		builder.delegatedUploadButtonResult = new HtmlPanelGroup();

		Button button = new Button();
		button.setPixelWidth(Integer.valueOf(80));
		button.setPixelHeight(Integer.valueOf(30));

		Action action = mock(Action.class);
		when(action.getName()).thenReturn("uploadContent");
		when(action.getClientValidation()).thenReturn(Boolean.TRUE);
		when(action.getDisabledConditionName()).thenReturn("uploadDisabled");
		when(action.getInvisibleConditionName()).thenReturn("uploadInvisible");

		UIComponent result = builder.uploadButton(null,
													"Upload",
													"pi pi-upload",
													"Upload file",
													"Are you sure?",
													button,
													"formDisabledUpload",
													action);

		assertSame(builder.delegatedUploadButtonResult, result);
		assertEquals("Upload", builder.uploadTitle);
		assertEquals("pi pi-upload", builder.uploadIconStyleClass);
		assertEquals("Upload file", builder.uploadTooltip);
		assertEquals("uploadContent", builder.uploadActionName);
		assertEquals(Integer.valueOf(80), builder.uploadPixelWidth);
		assertEquals(Integer.valueOf(30), builder.uploadPixelHeight);
		assertEquals(Boolean.TRUE, builder.uploadClientValidation);
		assertEquals("Are you sure?", builder.uploadConfirmationText);
		assertEquals("uploadDisabled", builder.uploadDisabled);
		assertEquals("formDisabledUpload", builder.uploadFormDisabled);
		assertEquals("uploadInvisible", builder.uploadInvisible);
		assertEquals(false, builder.uploadUseDialog);
	}

	@SuppressWarnings("static-method")
	@Test
	void testDownloadButtonNonShortcutPathUsesDefaultProcessAndUpdate() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		CommandButton commandButton = mock(CommandButton.class);
		MethodExpression actionExpression = mock(MethodExpression.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("downloadButtonId");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(CommandButton.COMPONENT_TYPE)).thenReturn(commandButton);
		when(mockExpressionFactory.createMethodExpression(any(ELContext.class), anyString(), isNull(), any(Class[].class))).thenReturn(actionExpression);

		Action action = mock(Action.class);
		when(action.getName()).thenReturn("downloadCsv");
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		Map<String, String> properties = new HashMap<>();
		when(action.getProperties()).thenReturn(properties);

		Button button = new Button();
		button.setPixelWidth(Integer.valueOf(90));
		button.setPixelHeight(Integer.valueOf(28));

		UIComponent result = builder.downloadButton(null,
													"orders",
													"row",
													"Download",
													"pi pi-download",
													"Download row",
													null,
													button,
													null,
													action);

		assertSame(commandButton, result);
		verify(commandButton).setValue("Download");
		verify(commandButton).setIcon("pi pi-download");
		verify(commandButton).setTitle("Download row");
		verify(commandButton).setProcess("@form");
		verify(commandButton).setUpdate("@(form)");
		verify(commandButton).setActionExpression(actionExpression);
	}

	@SuppressWarnings("static-method")
	@Test
	void testDownloadButtonNonShortcutPathUsesOverridesFromActionProperties() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		CommandButton commandButton = mock(CommandButton.class);
		MethodExpression actionExpression = mock(MethodExpression.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("downloadButtonId2");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(CommandButton.COMPONENT_TYPE)).thenReturn(commandButton);
		when(mockExpressionFactory.createMethodExpression(any(ELContext.class), anyString(), isNull(), any(Class[].class))).thenReturn(actionExpression);

		Action action = mock(Action.class);
		when(action.getName()).thenReturn("downloadPdf");
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		Map<String, String> properties = new HashMap<>();
		properties.put("process", "@this");
		properties.put("update", "@none");
		when(action.getProperties()).thenReturn(properties);

		Button button = new Button();

		UIComponent result = builder.downloadButton(null,
													null,
													"row",
													"Download",
													null,
													"Download",
													null,
													button,
													null,
													action);

		assertSame(commandButton, result);
		verify(commandButton).setProcess("@this");
		verify(commandButton).setUpdate("@none");
		verify(commandButton).setActionExpression(actionExpression);
	}

	@SuppressWarnings("static-method")
	@Test
	void testDataGridUsesCanAddEmptyMessageWhenEditableAndShowAddAreNotFalse() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		DataTable dataTable = mock(DataTable.class);
		UIOutput emptyMessage = mock(UIOutput.class);
		Map<String, UIComponent> facets = new HashMap<>();
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("gridId");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(DataTable.COMPONENT_TYPE)).thenReturn(dataTable);
		when(mockApplication.createComponent(UIOutput.COMPONENT_TYPE)).thenReturn(emptyMessage);
		when(dataTable.getFacets()).thenReturn(facets);

		DataGrid grid = new DataGrid();
		grid.setBinding("items");
		grid.setInline(Boolean.TRUE);
		grid.setEditable(null);
		grid.setShowAdd(null);

		UIComponent result = builder.dataGrid(null, "row", false, grid);

		assertSame(dataTable, result);
		assertSame(emptyMessage, facets.get("emptyMessage"));
		verify(emptyMessage).setValue(TabularComponentBuilder.EMPTY_DATA_TABLE_CAN_ADD_MESSAGE);
	}

	@SuppressWarnings("static-method")
	@Test
	void testDataGridUsesDefaultEmptyMessageWhenNotEditable() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		DataTable dataTable = mock(DataTable.class);
		UIOutput emptyMessage = mock(UIOutput.class);
		Map<String, UIComponent> facets = new HashMap<>();
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("gridId2");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(DataTable.COMPONENT_TYPE)).thenReturn(dataTable);
		when(mockApplication.createComponent(UIOutput.COMPONENT_TYPE)).thenReturn(emptyMessage);
		when(dataTable.getFacets()).thenReturn(facets);

		DataGrid grid = new DataGrid();
		grid.setBinding("items");
		grid.setInline(Boolean.TRUE);
		grid.setEditable(Boolean.FALSE);

		UIComponent result = builder.dataGrid(null, "row", false, grid);

		assertSame(dataTable, result);
		assertSame(emptyMessage, facets.get("emptyMessage"));
		verify(emptyMessage).setValue(TabularComponentBuilder.EMPTY_DATA_TABLE_MESSAGE);
	}

	@SuppressWarnings("static-method")
	@Test
	void testDataRepeaterNonShortcutPathSetsExpectedDataTableOptions() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		DataTable dataTable = mock(DataTable.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("repeaterId");
		builder.setManagedBeanForTest(managedBean);

		when(mockApplication.createComponent(DataTable.COMPONENT_TYPE)).thenReturn(dataTable);

		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("items");
		repeater.setShowColumnHeaders(Boolean.FALSE);
		repeater.setShowGrid(Boolean.FALSE);

		UIComponent result = builder.dataRepeater(null, "row", repeater);

		assertSame(dataTable, result);
		verify(dataTable).setEmptyMessage("");
		verify(dataTable).setStyleClass("repeater repeater-no-headers repeater-no-border");
		verify(dataTable).setReflow(true);
	}

	@SuppressWarnings("static-method")
	@Test
	void testAddDataGridBoundColumnNonShortcutPathAddsColumnWhenInlineEditable() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		Column createdColumn = mock(Column.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("gridBoundColumnId");
		builder.setManagedBeanForTest(managedBean);
		when(mockApplication.createComponent(Column.COMPONENT_TYPE)).thenReturn(createdColumn);

		UIComponent current = mock(UIComponent.class);
		List<UIComponent> currentChildren = new ArrayList<>();
		when(current.getChildren()).thenReturn(currentChildren);
		DataGrid widget = new DataGrid();
		widget.setInline(Boolean.TRUE);
		DataGridBoundColumn column = new DataGridBoundColumn();
		column.setEditable(Boolean.TRUE);
		StringBuilder gridColumnExpression = new StringBuilder("unchanged");

		UIComponent result = builder.addDataGridBoundColumn(null,
															current,
															widget,
															column,
															"row",
															"Name",
															"name",
															gridColumnExpression,
															null,
															null);

		assertSame(createdColumn, result);
		assertSame(createdColumn, currentChildren.get(0));
		assertEquals("unchanged", gridColumnExpression.toString());
		verify(createdColumn).setHeaderText("Name");
	}

	@SuppressWarnings("static-method")
	@Test
	void testAddedDataGridBoundColumnNonShortcutPathReturnsParentWhenNoChildren() {
		TabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		UIComponent current = mock(UIComponent.class);
		UIComponent parent = mock(UIComponent.class);
		when(current.getChildren()).thenReturn(new ArrayList<>());
		when(current.getParent()).thenReturn(parent);

		UIComponent result = builder.addedDataGridBoundColumn(null, current, null);

		assertSame(parent, result);
	}

	@SuppressWarnings("static-method")
	@Test
	void testAddDataGridContainerColumnNonShortcutPathAddsColumn() {
		NoOpTabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		Column createdColumn = mock(Column.class);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("gridContainerColumnId");
		builder.setManagedBeanForTest(managedBean);
		when(mockApplication.createComponent(Column.COMPONENT_TYPE)).thenReturn(createdColumn);

		UIComponent current = mock(UIComponent.class);
		List<UIComponent> currentChildren = new ArrayList<>();
		when(current.getChildren()).thenReturn(currentChildren);
		DataGrid widget = new DataGrid();
		widget.setBinding("items");
		DataGridContainerColumn containerColumn = new DataGridContainerColumn();

		UIComponent result = builder.addDataGridContainerColumn(null,
																current,
																widget,
																"Details",
																containerColumn,
																null);

		assertSame(createdColumn, result);
		assertSame(createdColumn, currentChildren.get(0));
		verify(createdColumn).setHeaderText("Details");
	}

	@SuppressWarnings("static-method")
	@Test
	void testAddedDataGridContainerColumnNonShortcutPathReturnsParent() {
		TabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		UIComponent current = mock(UIComponent.class);
		UIComponent parent = mock(UIComponent.class);
		when(current.getParent()).thenReturn(parent);

		UIComponent result = builder.addedDataGridContainerColumn(null, current);

		assertSame(parent, result);
	}

	@SuppressWarnings("static-method")
	@Test
	void testAddDataGridActionColumnNonShortcutPathReturnsCurrentWhenGridNotEditable() {
		TabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		UIComponent current = mock(UIComponent.class);
		DataGrid grid = new DataGrid();
		grid.setEditable(Boolean.FALSE);

		UIComponent result = builder.addDataGridActionColumn(null, current, grid, "row", "{name}", "Item", false, true, true);

		assertSame(current, result);
	}

	@SuppressWarnings("static-method")
	@Test
	void testSidebarScriptPixelWidthAndDerivedFloatingWidth() {
		TabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		Sidebar sidebar = new Sidebar();
		sidebar.setPixelWidth(Integer.valueOf(480));
		sidebar.setFloatingPixelWidthBreakpoint(Integer.valueOf(900));

		UIOutput result = (UIOutput) builder.sidebarScript(null, sidebar, false, "sb2");
		String script = (String) result.getValue();

		assertNotNull(script);
		assertTrue(script.contains("SKYVE.PF.sidebar('sb2','480px',900,480,'Edit')"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testSidebarScriptResponsiveAndPercentageWidths() {
		TabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		Sidebar responsiveSidebar = new Sidebar();
		responsiveSidebar.setResponsiveWidth(Integer.valueOf(6));

		UIOutput responsiveResult = (UIOutput) builder.sidebarScript(null, responsiveSidebar, true, "sb3");
		String responsiveScript = (String) responsiveResult.getValue();
		assertNotNull(responsiveScript);
		assertTrue(responsiveScript.contains("SKYVE.PF.sidebar('sb3','50%',1280,360,'Create')"));

		Sidebar percentageSidebar = new Sidebar();
		percentageSidebar.setPercentageWidth(Integer.valueOf(75));

		UIOutput percentageResult = (UIOutput) builder.sidebarScript(null, percentageSidebar, true, "sb4");
		String percentageScript = (String) percentageResult.getValue();
		assertNotNull(percentageScript);
		assertTrue(percentageScript.contains("SKYVE.PF.sidebar('sb4','75%',1280,360,'Create')"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testTabPaneScriptWithoutSelectedTabBinding() {
		TabularComponentBuilder builder = new NoOpTabularComponentBuilder();
		TabPane tabPane = new TabPane();

		UIOutput result = (UIOutput) builder.tabPaneScript(null, tabPane, "admin", "User", "tp1");
		String script = (String) result.getValue();

		assertNotNull(script);
		assertTrue(script.contains("sessionStorage.tab_admin_User_tp1"));
		assertTrue(script.contains("t.select("));
	}

	private static Object defaultValue(Class<?> type) {
		if (! type.isPrimitive()) {
			return null;
		}
		if (boolean.class.equals(type)) {
			return Boolean.FALSE;
		}
		/*if (char.class.equals(type)) {
			return Character.valueOf('\0');
		}
		if (byte.class.equals(type)) {
			return Byte.valueOf((byte) 0);
		}
		if (short.class.equals(type)) {
			return Short.valueOf((short) 0);
		}
		if (int.class.equals(type)) {
			return Integer.valueOf(0);
		}
		if (long.class.equals(type)) {
			return Long.valueOf(0L);
		}
		if (float.class.equals(type)) {
			return Float.valueOf(0f);
		}
		if (double.class.equals(type)) {
			return Double.valueOf(0d);
		}*/
		throw new IllegalArgumentException("Unsupported primitive type " + type);
	}

	private static AbstractPersistence currentPersistenceIfPresent() {
		return AbstractPersistence.isPresent() ? AbstractPersistence.get() : null;
	}

	@SuppressWarnings("unchecked")
	private static void restorePersistence(AbstractPersistence persistence) {
		if (persistence != null) {
			persistence.setForThread();
			return;
		}
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
		}
		catch (ReflectiveOperationException e) {
			throw new RuntimeException("Unable to restore thread-local persistence", e);
		}
	}
}
