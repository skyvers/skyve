package util.sail;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * The roles that the views are built with for each user a SAIL script signs in as.
 * <p>
 * The interpreted executors rebuild views in the test JVM to locate widgets. By default they build them as a user
 * with every role ({@link SailUsers#bootstrapLikeUser}). A script that signs in as a user with fewer roles would then
 * locate widgets that the user does not see. The test JVM cannot read the server's users, so a test registers the
 * roles of each such user here, for example:
 *
 * <pre>
 * viewRoles("client", "todo.ToDoClient");
 * viewRoles("tim", "timesheet.TimesheetUser", "admin.BasicUser");
 * sailResource("sail/apps/todo.xml");
 * </pre>
 *
 * On each {@code <login/>}, the executor builds the views for the rest of the run as a user with exactly the
 * registered roles of the user signing in (see {@link SailUsers#userWithRoles}), or as the every-role user if the
 * user is not registered. A role is named {@code module.Role}, or by a customer role's name.
 * <p>
 * A registered user name is matched against the login's {@code user} as written in the script, then with
 * {@code {run}} and {@code {var:name}} substituted. So {@code viewRoles("jane{run}", "admin.BasicUser")} matches a user
 * that the script created with the run token.
 * <p>
 * Not thread-safe: a test registers its users before it runs a script.
 */
public final class SailViewRoles {
	private final Map<String, List<String>> roleNamesByUserName = new HashMap<>();

	/**
	 * Registers the roles of a user, replacing any roles registered for the user before.
	 *
	 * @param userName	the user name as the script signs in with it
	 * @param roleNames	the user's roles: {@code module.Role} or a customer role's name; at least one
	 * @return this, for chaining
	 * @throws IllegalArgumentException	if there are no role names, or one is blank
	 */
	public @Nonnull SailViewRoles register(@Nonnull String userName, @Nonnull String... roleNames) {
		if (roleNames.length == 0) {
			throw new IllegalArgumentException("Register at least one role for " + userName);
		}
		for (String roleName : roleNames) {
			if ((roleName == null) || roleName.isBlank()) {
				throw new IllegalArgumentException("A role of " + userName + " is blank");
			}
		}
		roleNamesByUserName.put(userName, List.of(roleNames));
		return this;
	}

	/**
	 * Returns the roles registered for a signing in user.
	 *
	 * @param scriptUserName	the login's user as written in the script
	 * @param userName	the login's user with the tokens substituted
	 * @return the role names, or null if neither name is registered
	 */
	public @Nullable List<String> roleNames(@Nonnull String scriptUserName, @Nonnull String userName) {
		List<String> result = roleNamesByUserName.get(scriptUserName);
		return (result == null) ? roleNamesByUserName.get(userName) : result;
	}
}
