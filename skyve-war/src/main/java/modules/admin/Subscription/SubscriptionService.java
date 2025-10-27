package modules.admin.Subscription;

import java.util.Date;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.util.UUIDv7;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;

import jakarta.enterprise.inject.Default;
import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.Subscription;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient SubscriptionService subscriptionService;
 */
@Default
public class SubscriptionService {
	private static final String SUBSCRIPTION_PUBLIC_USERNAME = "SkyveSubscriptionUser";
	private static final String SUBSCRIPTION_PUBLIC_USER_ID = "SkyveSubscriptionUser";

	/**
	 * Creates a declined subscription record for an anonymous unsubscribe request.
	 * This method allows users to unsubscribe from communications without requiring authentication.
	 * It first checks if a public subscription user exists for the customer and creates the
	 * unsubscribe record if the public user is available.
	 * 
	 * @param persistence the persistence context for database operations
	 * @param bizCustomer the bizCustomer identifier
	 * @param communicationId the unique identifier of the communication to unsubscribe from
	 * @param receiverIdentifier the identifier of the receiver (e.g., email address)
	 * @return {@code true} if the unsubscribe was successful, {@code false} otherwise
	 * @throws Exception if there's an error during the database operation
	 */
	public boolean anonymouslyUnsubscribe(Persistence persistence, String bizCustomer, String communicationId,
			String receiverIdentifier) throws Exception {
		boolean success = false;

		boolean hasPublicUser = anonymouslyCheckSubscriptionPublicUser(persistence, bizCustomer);
		if (hasPublicUser) {

			String sqlInsertString = """
					insert into ADM_Subscription (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId,
						communication_id, receiverIdentifier, declined)
					values (:bizId, :bizVersion, :bizLock, :bizKey, :bizCustomer, :bizUserId,
						:communication, :receiverIdentifier, :declined)
					""";

			SQL sqlInsert = persistence.newSQL(sqlInsertString);

			String bizId = UUIDv7.create().toString();
			Integer bizVersion = Integer.valueOf(0);
			OptimisticLock lock = new OptimisticLock(SUBSCRIPTION_PUBLIC_USERNAME, new Date());
			String bizLock = lock.toString();
			String bizKey = "Subscription for " + receiverIdentifier;
			String bizUserId = SUBSCRIPTION_PUBLIC_USER_ID;

			sqlInsert.putParameter(Bean.DOCUMENT_ID, bizId, false);
			sqlInsert.putParameter(PersistentBean.VERSION_NAME, bizVersion);
			sqlInsert.putParameter(PersistentBean.LOCK_NAME, bizLock, false);
			sqlInsert.putParameter(Bean.BIZ_KEY, bizKey, false);
			sqlInsert.putParameter(Bean.CUSTOMER_NAME, bizCustomer, false);
			sqlInsert.putParameter(Bean.USER_ID, bizUserId, false);
			sqlInsert.putParameter(Subscription.communicationPropertyName, communicationId, false);
			sqlInsert.putParameter(Subscription.receiverIdentifierPropertyName, receiverIdentifier, false);
			sqlInsert.putParameter(Subscription.declinedPropertyName, Boolean.TRUE);

			sqlInsert.execute();
		}

		return success;
	}

	/**
	 * Checks for the existence of a subscription public user and creates one if it doesn't exist.
	 * If the public user doesn't exist, this method will create both the contact and security
	 * user records within a transaction.
	 * 
	 * @param persistence the persistence context for database operations
	 * @param bizCustomer the bizCustomer identifier for which to check/create the public user
	 * @return {@code true} if the public user exists or was successfully created, {@code false} otherwise
	 */
	@SuppressWarnings("static-method")
	public boolean anonymouslyCheckSubscriptionPublicUser(Persistence persistence, String bizCustomer) {
		boolean result = false;

		String s = """
				select bizId, userName
				from ADM_SecurityUser
				where bizCustomer = :%s
				and bizId = :%s
				""".formatted(Bean.CUSTOMER_NAME, Bean.DOCUMENT_ID);

		SQL sql = persistence.newSQL(s);
		sql.putParameter(Bean.CUSTOMER_NAME, bizCustomer, false);
		sql.putParameter(Bean.DOCUMENT_ID, SUBSCRIPTION_PUBLIC_USER_ID, false);

		try {
			Object[] row = sql.tupleResult();
			if (row == null) {

				// handle within transaction
				persistence.begin();

				// attempt to create the public user
				String PUBLIC_USER_CONTACT_ID = SUBSCRIPTION_PUBLIC_USER_ID;

				// create the contact
				String sqlInsContactString = """
						insert into ADM_Contact (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId,
							name, contactType)
						values (:bizId, :bizVersion, :bizLock, :bizKey, :bizCustomer, :bizUserId,
							:name, :contactType)
						""";

				SQL sqlInsContact = persistence.newSQL(sqlInsContactString);

				String bizId = SUBSCRIPTION_PUBLIC_USER_ID;
				Integer bizVersion = Integer.valueOf(0);
				OptimisticLock lock = new OptimisticLock(SUBSCRIPTION_PUBLIC_USERNAME, new Date());
				String bizLock = lock.toString();
				String bizKey = SUBSCRIPTION_PUBLIC_USERNAME;
				String bizUserId = SUBSCRIPTION_PUBLIC_USER_ID;

				sqlInsContact.putParameter(Bean.DOCUMENT_ID, bizId, false);
				sqlInsContact.putParameter(PersistentBean.VERSION_NAME, bizVersion);
				sqlInsContact.putParameter(PersistentBean.LOCK_NAME, bizLock, false);
				sqlInsContact.putParameter(Bean.BIZ_KEY, bizKey, false);
				sqlInsContact.putParameter(Bean.CUSTOMER_NAME, bizCustomer, false);
				sqlInsContact.putParameter(Bean.USER_ID, bizUserId, false);
				sqlInsContact.putParameter(Contact.namePropertyName, SUBSCRIPTION_PUBLIC_USERNAME, false);
				sqlInsContact.putParameter(Contact.contactTypePropertyName, ContactType.person);

				sqlInsContact.execute();

				// insert a public user with no password so that they cannot log in
				String sqlInsUserString = """
						insert into ADM_SecurityUser (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId,
							userName, contact_id)
						values (:bizId, :bizVersion, :bizLock, :bizKey, :bizCustomer, :bizUserId,
							:userName, :contact)
						""";

				SQL sqlInsUser = persistence.newSQL(sqlInsUserString);

				bizId = SUBSCRIPTION_PUBLIC_USER_ID;
				bizVersion = Integer.valueOf(0);
				lock = new OptimisticLock(SUBSCRIPTION_PUBLIC_USERNAME, new Date());
				bizLock = lock.toString();
				bizKey = SUBSCRIPTION_PUBLIC_USERNAME;
				bizUserId = SUBSCRIPTION_PUBLIC_USER_ID;

				sqlInsUser.putParameter(Bean.DOCUMENT_ID, bizId, false);
				sqlInsUser.putParameter(PersistentBean.VERSION_NAME, bizVersion);
				sqlInsUser.putParameter(PersistentBean.LOCK_NAME, bizLock, false);
				sqlInsUser.putParameter(Bean.BIZ_KEY, bizKey, false);
				sqlInsUser.putParameter(Bean.CUSTOMER_NAME, bizCustomer, false);
				sqlInsUser.putParameter(Bean.USER_ID, bizUserId, false);
				sqlInsUser.putParameter(modules.admin.domain.User.userNamePropertyName, SUBSCRIPTION_PUBLIC_USERNAME, false);
				sqlInsUser.putParameter(modules.admin.domain.User.contactPropertyName, PUBLIC_USER_CONTACT_ID, false);
				sqlInsUser.execute();

				persistence.commit(false);
				result = true;
			} else {
				result = true;
			}
		} catch (@SuppressWarnings("unused") DomainException d) {
			// do nothing, return null
			// public user cannot be found or created
		}

		return result;
	}

	/**
	 * Anonymously checks whether a subscription record exists for the specified parameters.
	 * This method queries the subscription table to determine if there's already a subscription
	 * entry for the given customer, communication, and receiver combination. This is useful
	 * for preventing duplicate subscription records and validating subscription status.
	 * 
	 * @param p the persistence context for database operations
	 * @param bizCustomer the bizCustomer identifier
	 * @param communicationId the unique identifier of the communication
	 * @param receiverIdentifier the identifier of the receiver (e.g., email address)
	 * @return {@code true} if a subscription exists for the given parameters, {@code false} otherwise
	 */
	@SuppressWarnings("static-method")
	public boolean anonymouslySubscriptionExists(Persistence p, String bizCustomer, String communicationId,
			String receiverIdentifier) {

		boolean result = false;

		String sqlSubString = """
				select count(*) from ADM_Subscription
				where communication_id = :%s
				and receiverIdentifier = :%s
				and bizCustomer = :%s
				""".formatted(Subscription.communicationPropertyName, Subscription.receiverIdentifierPropertyName,
				Bean.CUSTOMER_NAME);

		SQL sqlSub = p.newSQL(sqlSubString);
		sqlSub.putParameter(Bean.CUSTOMER_NAME, bizCustomer, false);
		sqlSub.putParameter(Subscription.communicationPropertyName, communicationId, false);
		sqlSub.putParameter(Subscription.receiverIdentifierPropertyName, receiverIdentifier, false);

		// get results
		try {
			Number exists = sqlSub.scalarResult(Number.class);
			result = (exists != null) && (exists.longValue() > 0);
		} catch (@SuppressWarnings("unused") DomainException d) {
			// do nothing, return false
		}
		return result;
	}
}
