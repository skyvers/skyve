package modules.admin.UserAccount;

import org.skyve.util.test.SkyveFactory;

import modules.admin.UserAccount.actions.SignOutOtherSessions;

/**
 * Stops generateDomain generating an action test for {@link SignOutOtherSessions}, which needs an HTTP request
 * (the cookie and session to keep) that a generated test does not give it. The exclusion lives here, not in a
 * hand-written test of the same name, so that assembled applications get it too.
 */
@SkyveFactory(excludedActions = { SignOutOtherSessions.class })
public class UserAccountFactory {
	// builder defaults
}
