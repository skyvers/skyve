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
	void setDisabledConditionNameIsStoredAndRetrieved() {
		SetDisabledEventAction action = new SetDisabledEventAction();
		action.setDisabledConditionName("disabled");
		assertEquals("disabled", action.getDisabledConditionName());
		assertNull(action.getEnabledConditionName());
		action.getProperties().put("k", "v");
		assertEquals("v", action.getProperties().get("k"));
	}

	/** Verifies enabled conditions are negated and blank or null inputs normalize to null. */
	@Test
	void setDisabledNegatesEnabledExpressionAndTrimsBlankInput() {
		SetDisabledEventAction action = new SetDisabledEventAction();
		action.setEnabledConditionName("enabled");
		assertEquals("notEnabled", action.getDisabledConditionName());
		action.setEnabledConditionName("notReady");
		assertEquals("ready", action.getDisabledConditionName());
		action.setEnabledConditionName("   ");
		assertNull(action.getDisabledConditionName());
		action.setEnabledConditionName(null);
		assertNull(action.getDisabledConditionName());
	}

	/** Verifies invisible condition names can be stored directly and retrieved unchanged. */
	@Test
	void setInvisibleConditionNameIsStoredAndRetrieved() {
		SetInvisibleEventAction action = new SetInvisibleEventAction();
		action.setInvisibleConditionName("hidden");
		assertEquals("hidden", action.getInvisibleConditionName());
		assertNull(action.getVisibleConditionName());
		action.getProperties().put("k", "v");
		assertEquals("v", action.getProperties().get("k"));
	}

	/** Verifies visible conditions are negated and blank or null inputs normalize to null. */
	@Test
	void setInvisibleNegatesVisibleExpressionAndTrimsBlankInput() {
		SetInvisibleEventAction action = new SetInvisibleEventAction();
		action.setVisibleConditionName("visible");
		assertEquals("notVisible", action.getInvisibleConditionName());
		action.setVisibleConditionName("notShown");
		assertEquals("shown", action.getInvisibleConditionName());
		action.setVisibleConditionName("   ");
		assertNull(action.getInvisibleConditionName());
		action.setVisibleConditionName(null);
		assertNull(action.getInvisibleConditionName());
	}

	/** Verifies rerender action supports explicitly disabling client-side validation. */
	@Test
	void rerenderSupportsFalseClientValidation() {
		RerenderEventAction action = new RerenderEventAction();
		action.setClientValidation(Boolean.FALSE);
		assertFalse(action.getClientValidation().booleanValue());
	}
}
