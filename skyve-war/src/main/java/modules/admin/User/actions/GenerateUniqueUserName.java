package modules.admin.User.actions;

import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.domain.User;

/**
 * Generates a unique username candidate derived from contact name data.
 */
public class GenerateUniqueUserName implements ServerSideAction<User> {
	/**
	 * Sets a generated unique username on the user bean.
	 *
	 * @param user The user bean being edited.
	 * @param webContext The current web context.
	 * @return The same user bean.
	 * @throws Exception If username generation fails.
	 */
	@Override
	public ServerSideActionResult<User> execute(User user, WebContext webContext) throws Exception {

		user.setUserName(generateUniqueUserNameFromContactName(user));

		return new ServerSideActionResult<>(user);
	}

	/**
	 * Builds a username stem from contact name and resolves collisions by incrementing a trailing number.
	 *
	 * @param user The user requiring a generated username.
	 * @return A unique username suggestion.
	 * @throws Exception If lookup validation fails.
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	public static String generateUniqueUserNameFromContactName(User user) throws Exception {
		if (user.getContact() == null) {
			Message vm = new Message(User.contactPropertyName, "You first need to select a contact for this user");
			ValidationException ve = new ValidationException(vm);
			throw ve;
		}
		String newUName = "";

		if (user.getContact() != null && user.getContact().getName() != null) {
			newUName = user.getContact().getName().toLowerCase().replaceAll("[^a-z]", "");
			if (newUName.length() > 10) {
				newUName = newUName.substring(0, 9);
			}
		}

		Persistence persistence = CORE.getPersistence();
		DocumentQuery q = persistence.newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		q.getFilter().addLike(User.userNamePropertyName, newUName + "%");
		q.addAggregateProjection(AggregateFunction.Max, User.userNamePropertyName, "MaxOfUserName");

		List<String> results = q.scalarResults(String.class);
		if (results.size() > 0) {
			String maxUName = results.get(0);

			if (maxUName != null) {
				// go backwards and find trailing numeric
				Integer numericPart = Integer.valueOf(0);
				for (int i = maxUName.length() - 1; i > 0; i--) {
					try {
						// see if last chars are numeric
						numericPart = Integer.valueOf(Integer.parseInt(maxUName.substring(i, maxUName.length())));
					} catch (@SuppressWarnings("unused") Exception e) {
						// break out when non-numeric found
						break;
					}
				}

				if (numericPart.equals(Integer.valueOf(0))) {
					// no previous matches - just append a numeric
					newUName = maxUName + "1";
				} else {
					// previous matches found - increment the numeric
					newUName = maxUName.replace(numericPart.toString(), (Integer.valueOf(numericPart.intValue() + 1)).toString());
				}

			}
		}
		return newUName;
	}
}
