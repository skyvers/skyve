package org.skyve.impl.web.faces.pipeline.component;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.primefaces.behavior.confirm.ConfirmBehavior;
import org.primefaces.component.commandbutton.CommandButton;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.bound.input.ContentCapture;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.Action;
import org.skyve.web.UserAgentType;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.el.MethodExpression;
import jakarta.el.ValueExpression;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;

class DeviceResponsiveComponentBuilderTest {

	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext facesContext) {
			setCurrentInstance(facesContext);
		}
	}

	private static Application mockApplication;
	private static ExpressionFactory mockExpressionFactory;

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

	// ---- spacer ----

	@Test
	@SuppressWarnings("static-method")
	void spacerReturnsExistingComponentWhenNotNull() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.phone);
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.spacer(existing, new Spacer()));
	}

	@Test
	@SuppressWarnings("static-method")
	void spacerReturnsNullForPhone() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.phone);
		assertNull(builder.spacer(null, new Spacer()));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void spacerDelegatesToSuperForNonPhone() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.desktop);
		// Super call creates a Spacer component — mock the Application.createComponent call
		org.primefaces.component.spacer.Spacer mockSpacer = mock(org.primefaces.component.spacer.Spacer.class);
		when(mockApplication.createComponent(anyString())).thenReturn(mockSpacer);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(String.class)))
			.thenReturn(mock(jakarta.el.ValueExpression.class));		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("spacer1");
		builder.setSAILManagedBean(managedBean);		UIComponent result = builder.spacer(null, new Spacer());
		assertSame(mockSpacer, result);
	}

	// ---- upload short-circuit ----

	@Test
	@SuppressWarnings("static-method")
	void uploadReturnsExistingComponentWhenNotNull() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.phone);
		UIComponent existing = mock(UIComponent.class);
		Action action = mock(Action.class);
		assertSame(existing, builder.upload(existing, "label", "icon", "tip", "confirm", action));
	}

	// ---- uploadButton short-circuit ----

	@Test
	@SuppressWarnings("static-method")
	void uploadButtonReturnsExistingComponentWhenNotNull() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.phone);
		UIComponent existing = mock(UIComponent.class);
		Button button = new Button();
		Action action = mock(Action.class);
		assertSame(existing, builder.uploadButton(existing, "label", "icon", "tip", "confirm", button, null, action));
	}

	// ---- upload delegates with phone flag ----

	@Test
	@SuppressWarnings({"static-method"})
	void uploadCallsUploadButtonWithPhoneFlagTrue() {
		// Use a capturing subclass to verify the useDialog flag
		final boolean[] capturedUseDialog = {false};
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder() {
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
												ContentCapture capture,
												boolean useDialog) {
				capturedUseDialog[0] = useDialog;
				return mock(UIComponent.class);
			}
		};
		builder.setUserAgentType(UserAgentType.phone);
		Action action = mock(Action.class);
		when(action.getName()).thenReturn("save");
		when(action.getClientValidation()).thenReturn(Boolean.TRUE);
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		builder.upload(null, "Upload", "icon", "tip", null, action);
		assertTrue(capturedUseDialog[0], "Expected useDialog=true for phone");
	}

	@Test
	@SuppressWarnings({"static-method"})
	void uploadCallsUploadButtonWithPhoneFlagFalse() {
		final boolean[] capturedUseDialog = {true};
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder() {
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
												ContentCapture capture,
												boolean useDialog) {
				capturedUseDialog[0] = useDialog;
				return mock(UIComponent.class);
			}
		};
		builder.setUserAgentType(UserAgentType.desktop);
		Action action = mock(Action.class);
		when(action.getName()).thenReturn("save");
		when(action.getClientValidation()).thenReturn(Boolean.FALSE);
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		builder.upload(null, "Upload", "icon", "tip", null, action);
		assertFalse(capturedUseDialog[0], "Expected useDialog=false for desktop");
	}

	// ---- uploadButton delegates with phone flag ----

	@Test
	@SuppressWarnings({"static-method"})
	void uploadButtonCallsUploadButtonWithPhoneFlagTrue() {
		final boolean[] capturedUseDialog = {false};
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder() {
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
												ContentCapture capture,
												boolean useDialog) {
				capturedUseDialog[0] = useDialog;
				return mock(UIComponent.class);
			}
		};
		builder.setUserAgentType(UserAgentType.phone);
		Button button = new Button();
		Action action = mock(Action.class);
		when(action.getName()).thenReturn("save");
		when(action.getClientValidation()).thenReturn(null);
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		builder.uploadButton(null, "Upload", "icon", "tip", null, button, null, action);
		assertTrue(capturedUseDialog[0], "Expected useDialog=true for phone");
	}

	@Test
	@SuppressWarnings({"static-method"})
	void uploadButtonCallsUploadButtonWithPhoneFlagFalse() {
		final boolean[] capturedUseDialog = {true};
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder() {
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
												ContentCapture capture,
												boolean useDialog) {
				capturedUseDialog[0] = useDialog;
				return mock(UIComponent.class);
			}
		};
		builder.setUserAgentType(UserAgentType.desktop);
		Button button = new Button();
		Action action = mock(Action.class);
		when(action.getName()).thenReturn("save");
		when(action.getClientValidation()).thenReturn(null);
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		builder.uploadButton(null, "Upload", "icon", "tip", null, button, null, action);
		assertFalse(capturedUseDialog[0], "Expected useDialog=false for desktop");
	}

	// ---- actionButton phone branch ----

	@Test
	@SuppressWarnings("static-method")
	void actionButtonForPhoneCallsThrough() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.phone);
		Button button = new Button();
		button.setPixelWidth(Integer.valueOf(200));
		button.setPixelHeight(Integer.valueOf(50));
		Action action = mock(Action.class);
		when(action.getName()).thenReturn("Save");
		when(action.getImplicitName()).thenReturn(ImplicitActionName.Save);
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		CommandButton btn = mock(CommandButton.class);
		when(mockApplication.createComponent(CommandButton.COMPONENT_TYPE)).thenReturn(btn);
		ConfirmBehavior confirmBehavior = mock(ConfirmBehavior.class);
		when(mockApplication.createBehavior(ConfirmBehavior.BEHAVIOR_ID)).thenReturn(confirmBehavior);
		MethodExpression me = mock(MethodExpression.class);
		ValueExpression ve = mock(ValueExpression.class);
		when(mockExpressionFactory.createMethodExpression(any(ELContext.class), anyString(), any(), any()))
			.thenReturn(me);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), any(Class.class)))
			.thenReturn(ve);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("btnId");
		builder.setSAILManagedBean(managedBean);
		UIComponent result = builder.actionButton(null, null, null, "Save", null, null, null, button, null, action);
		Assertions.assertNotNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void actionButtonForDesktopCallsThrough() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.desktop);
		Button button = new Button();
		button.setPixelWidth(Integer.valueOf(200));
		button.setPixelHeight(Integer.valueOf(50));
		Action action = mock(Action.class);
		when(action.getName()).thenReturn("Save");
		when(action.getImplicitName()).thenReturn(ImplicitActionName.Save);
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		CommandButton btn = mock(CommandButton.class);
		when(mockApplication.createComponent(CommandButton.COMPONENT_TYPE)).thenReturn(btn);
		ConfirmBehavior confirmBehavior = mock(ConfirmBehavior.class);
		when(mockApplication.createBehavior(ConfirmBehavior.BEHAVIOR_ID)).thenReturn(confirmBehavior);
		MethodExpression me = mock(MethodExpression.class);
		ValueExpression ve = mock(ValueExpression.class);
		when(mockExpressionFactory.createMethodExpression(any(ELContext.class), anyString(), any(), any()))
			.thenReturn(me);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), any(Class.class)))
			.thenReturn(ve);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("btnId");
		builder.setSAILManagedBean(managedBean);
		UIComponent result = builder.actionButton(null, null, null, "Save", null, null, null, button, null, action);
		Assertions.assertNotNull(result);
	}
}
