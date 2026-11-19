package org.skyve.impl.sail.recording;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.web.UserAgentType;

import modules.test.AbstractSkyveTest;
import util.sail.SailUsers;

/**
 * Tests that the Stop checks build the views of the UX/UI a recording targets: the Control Panel has a view of its own
 * for the external UX/UI (views/external/edit.xml), with an action the desktop view has not, and without some of the
 * desktop view's fields.
 */
class SailRecordingChecksH2Test extends AbstractSkyveTest {
	private static final Map<String, String> CLIENT_SAIL = Map.of("op", "action", "name", "DownloadClientSAIL", "view", "admin.ControlPanel");
	private static final Map<String, String> QUERY = Map.of("op", "dataEnter", "binding", "query", "value", "x", "view", "admin.ControlPanel");

	@AfterEach
	void restoreUser() {
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void theRecordingsUxUiViewsAreChecked() {
		// the views are built as the user, who has every role (as admin signs in to record)
		AbstractPersistence.get().setUser(SailUsers.bootstrapLikeUser(u));
		SailRecordingChecks external = new SailRecordingChecks("external", UserAgentType.desktop);
		SailRecordingChecks desktop = new SailRecordingChecks();
		assertAll(() -> assertNull(external.unconfirmed(CLIENT_SAIL), "the external view's own action"),
					() -> assertNotNull(external.unconfirmed(QUERY), "a desktop-only field is not on the external view"),
					() -> assertNotNull(desktop.unconfirmed(CLIENT_SAIL), "the external view's action is not on the desktop view"),
					() -> assertNull(desktop.unconfirmed(QUERY), "the desktop view's field"));
	}
}
