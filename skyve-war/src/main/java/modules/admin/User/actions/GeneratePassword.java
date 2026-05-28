package modules.admin.User.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.PasswordGenerator;
import modules.admin.domain.User;

/**
 * Generates and applies a temporary password for a user.
 */
public class GeneratePassword implements ServerSideAction<User> {
	/**
	 * Generates a random password and sets corresponding password fields on the bean.
	 *
	 * @param user The user bean to update.
	 * @param webContext The current web context.
	 * @return The same user bean.
	 * @throws Exception If password generation fails.
	 */
	@Override
	public ServerSideActionResult<User> execute(User user, WebContext webContext) throws Exception {

		generatePassword(user);

		return new ServerSideActionResult<>(user);
	}

	/**
	 * Updates temporary and confirmation password fields and marks password as expired.
	 *
	 * @param user The user bean receiving generated credentials.
	 * @throws Exception If generation fails.
	 */
	private static void generatePassword(User user) throws Exception {
		user.setGeneratedPassword(PasswordGenerator.generate());

		user.setNewPassword(user.getGeneratedPassword());
		user.setConfirmPassword(user.getGeneratedPassword());
		user.setPasswordExpired(Boolean.TRUE);
	}
}
