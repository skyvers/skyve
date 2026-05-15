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

	@Test
	void toggleVisibilityPropertiesAreMutable() {
		ToggleVisibilityEventAction action = new ToggleVisibilityEventAction();
		action.getProperties().put("a", "b");
		assertEquals("b", action.getProperties().get("a"));
	}

	@Test
	void toggleDisabledPropertiesAreMutable() {
		ToggleDisabledEventAction action = new ToggleDisabledEventAction();
		action.getProperties().put("a", "b");
		assertEquals("b", action.getProperties().get("a"));
	}

	@Test
	void serverSideActionNameRoundTripAndProperties() {
		ServerSideActionEventAction action = new ServerSideActionEventAction();
		assertNull(action.getActionName());
		action.setActionName("doSomething");
		action.getProperties().put("k", "v");
		assertEquals("doSomething", action.getActionName());
		assertEquals("v", action.getProperties().get("k"));
	}

	@Test
	void rerenderClientValidationRoundTripAndProperties() {
		RerenderEventAction action = new RerenderEventAction();
		assertNull(action.getClientValidation());
		action.setClientValidation(Boolean.TRUE);
		action.getProperties().put("k", "v");
		assertTrue(action.getClientValidation().booleanValue());
		assertEquals("v", action.getProperties().get("k"));
	}

	@Test
	void setDisabledConditionNameIsStoredAndRetrieved() {
		SetDisabledEventAction action = new SetDisabledEventAction();
		action.setDisabledConditionName("disabled");
		assertEquals("disabled", action.getDisabledConditionName());
		assertNull(action.getEnabledConditionName());
		action.getProperties().put("k", "v");
		assertEquals("v", action.getProperties().get("k"));
	}

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

	@Test
	void setInvisibleConditionNameIsStoredAndRetrieved() {
		SetInvisibleEventAction action = new SetInvisibleEventAction();
		action.setInvisibleConditionName("hidden");
		assertEquals("hidden", action.getInvisibleConditionName());
		assertNull(action.getVisibleConditionName());
		action.getProperties().put("k", "v");
		assertEquals("v", action.getProperties().get("k"));
	}

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

	@Test
	void rerenderSupportsFalseClientValidation() {
		RerenderEventAction action = new RerenderEventAction();
		action.setClientValidation(Boolean.FALSE);
		assertFalse(action.getClientValidation().booleanValue());
	}
}
