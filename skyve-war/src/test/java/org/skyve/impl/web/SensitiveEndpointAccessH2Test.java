package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.domain.messages.AccessException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;

import jakarta.servlet.http.HttpServletRequest;
import modules.test.AbstractSkyveTest;

@SuppressWarnings({ "java:S1192", "java:S5960" }) // Repeated literals and assertions are test-only fixtures.
class SensitiveEndpointAccessH2Test extends AbstractSkyveTest {
	@AfterEach
	void restorePersistenceUser() {
		AbstractPersistence.get().setUser(u);
	}

	@Test
	@SuppressWarnings("boxing")
	void deniedDynamicResourceRemainsDeniedUnderEmulation() {
		UxUi phone = UxUi.newPrimeFaces("phone", "external", "phone-theme");
		HttpServletRequest request = mock(HttpServletRequest.class);
		RequestUxUiSelection selection = new RequestUxUiSelection(org.skyve.web.UserAgentType.phone, true, phone);
		when(request.getAttribute(UserAgent.class.getName() + ".selection")).thenReturn(selection);

		User denied = spy(u);
		UserAccess access = UserAccess.dynamicImage("admin", "User", "portrait");
		doReturn(false).when(denied).canAccess(access, "phone");
		AbstractPersistence.get().setUser(denied);

		String uxuiName = selection.getUxUi().getName();
		assertTrue(selection.isEmulated());
		assertThrows(AccessException.class, () -> EXT.checkAccess(denied, access, uxuiName));
		verify(denied).canAccess(access, "phone");
	}
}
