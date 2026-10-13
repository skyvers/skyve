package org.skyve.impl.metadata.view.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Verifies basic state and condition-negation behaviour for metadata view event actions.
 *
 * <p>These tests intentionally focus on lightweight branch coverage for simple action metadata
 * types: property maps, field round-trips, and visible/enabled inversion semantics.
 */
@SuppressWarnings("static-method")
class EventActionTest {

	/** Verifies toggle visibility actions expose a mutable properties map. */
	@Test
	void toggleVisibilityPropertiesAreMutable() {
		ToggleVisibilityEventAction action = new ToggleVisibilityEventAction();
		action.getProperties().put("a", "b");
		assertEquals("b", action.getProperties().get("a"));
	}

	/** Verifies toggle disabled actions expose a mutable properties map. */
	@Test
	void toggleDisabledPropertiesAreMutable() {
		ToggleDisabledEventAction action = new ToggleDisabledEventAction();
		action.getProperties().put("a", "b");
		assertEquals("b", action.getProperties().get("a"));
	}

	/** Verifies server-side action name state and properties round-trip correctly. */
	@Test
	void serverSideActionNameRoundTripAndProperties() {
		ServerSideActionEventAction action = new ServerSideActionEventAction();
		assertNull(action.getActionName());
		action.setActionName("doSomething");
		action.getProperties().put("k", "v");
		assertEquals("doSomething", action.getActionName());
		assertEquals("v", action.getProperties().get("k"));
	}

	/** Verifies rerender action client-validation and properties round-trip correctly. */
	@Test
	void rerenderClientValidationRoundTripAndProperties() {
		RerenderEventAction action = new RerenderEventAction();
		assertNull(action.getClientValidation());
		action.setClientValidation(Boolean.TRUE);
		action.getProperties().put("k", "v");
		assertTrue(action.getClientValidation().booleanValue());
		assertEquals("v", action.getProperties().get("k"));
	}

	/** Verifies disabled condition names can be stored directly and retrieved unchanged. */
	@Test
	void setDisabledConditionIsStoredAndRetrieved() {
		SetDisabledEventAction action = new SetDisabledEventAction();
		action.setXmlDisabledCondition("disabled");
		assertEquals("disabled", action.getDisabledCondition());
		assertNull(action.getXmlEnabledCondition());
		action.getProperties().put("k", "v");
		assertEquals("v", action.getProperties().get("k"));
	}

	/** Verifies enabled conditions are negated and blank or null inputs normalize to null. */
	@Test
	void setDisabledNegatesEnabledExpressionAndTrimsBlankInput() {
		SetDisabledEventAction action = new SetDisabledEventAction();
		action.setXmlEnabledCondition("enabled");
		assertEquals("notEnabled", action.getDisabledCondition());
		action.setXmlEnabledCondition("notReady");
		assertEquals("ready", action.getDisabledCondition());
		action.setXmlEnabledCondition("   ");
		assertNull(action.getDisabledCondition());
		action.setXmlEnabledCondition(null);
		assertNull(action.getDisabledCondition());
	}

	/** Verifies invisible condition names can be stored directly and retrieved unchanged. */
	@Test
	void setInvisibleConditionIsStoredAndRetrieved() {
		SetInvisibleEventAction action = new SetInvisibleEventAction();
		action.setXmlInvisibleCondition("hidden");
		assertEquals("hidden", action.getInvisibleCondition());
		assertNull(action.getXmlVisibleCondition());
		action.getProperties().put("k", "v");
		assertEquals("v", action.getProperties().get("k"));
	}

	/** Verifies visible conditions are negated and blank or null inputs normalize to null. */
	@Test
	void setInvisibleNegatesVisibleExpressionAndTrimsBlankInput() {
		SetInvisibleEventAction action = new SetInvisibleEventAction();
		action.setXmlVisibleCondition("visible");
		assertEquals("notVisible", action.getInvisibleCondition());
		action.setXmlVisibleCondition("notShown");
		assertEquals("shown", action.getInvisibleCondition());
		action.setXmlVisibleCondition("   ");
		assertNull(action.getInvisibleCondition());
		action.setXmlVisibleCondition(null);
		assertNull(action.getInvisibleCondition());
	}

	/** Verifies rerender action supports explicitly disabling client-side validation. */
	@Test
	void rerenderSupportsFalseClientValidation() {
		RerenderEventAction action = new RerenderEventAction();
		action.setClientValidation(Boolean.FALSE);
		assertFalse(action.getClientValidation().booleanValue());
	}
}
