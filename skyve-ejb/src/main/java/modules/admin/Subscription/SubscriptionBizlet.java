package modules.admin.Subscription;

import java.math.BigInteger;
import java.util.Date;
import java.util.UUID;

import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.Subscription;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;

public class SubscriptionBizlet extends Bizlet<Subscription> {

	private static final long serialVersionUID = -8006732196635189574L;

	private static final String SUBSCRIPTION_PUBLIC_USERNAME = "SkyveSubscriptionUser";

	private static final String SUBSCRIPTION_PUBLIC_USER_ID = "SkyveSubscriptionUser";

	/**
	 * Validation for Subscription
	 * 
	 * If the subscription is declined, the format type is not required. If the
	 * format type is supplied, then the communication is only declined for that
	 * format.
	 * 
	 * If the subscription is not declined, the format type is required, as this
	 * specifies the format preference for the communication.
	 * 
	 * If the subscription is not declined and has no formatType, the
	 * subscription can be deleted as it holds no value.
	 * 
	 */
	@Override
	public void validate(Subscription bean, ValidationException e) throws Exception {

		// get correct nomenclature for the user
		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(Subscription.MODULE_NAME);
		Document document = module.getDocument(customer, Subscription.DOCUMENT_NAME);

		if (!Boolean.TRUE.equals(bean.getDeclined())) {
			if (bean.getFormatType() == null) {
				StringBuilder msg = new StringBuilder(128);
				msg.append("A ").append(document.getLocalisedSingularAlias());
				msg.append(" requires a ").append(document.getAttribute(Subscription.formatTypePropertyName).getLocalisedDisplayName());
				msg.append(" unless the ").append(document.getLocalisedSingularAlias());
				msg.append(" has ").append(document.getAttribute(Subscription.declinedPropertyName).getLocalisedDisplayName()).append(" set.");
				e.getMessages().add(new Message(Subscription.formatTypePropertyName, msg.toString()));
			}
		}

		super.validate(bean, e);
	}

	/**
	 * anonymouslyUnsubscribe creates a declined subscription for an anonymous request
	 * 
	 * @param persistence
	 * @param bizCustomer
	 * @param communicationId
	 * @param receiverIdentifier
	 * @param declined
	 * @return
	 * @throws Exception
	 */
	public static boolean anonymouslyUnsubscribe(Persistence persistence, String bizCustomer, String communicationId, String receiverIdentifier) throws Exception {
		boolean success = false;

		boolean hasPublicUser = anonymouslyCheckSubscriptionPublicUser(persistence, bizCustomer);
		if (hasPublicUser) {

			StringBuilder sqlInsertString = new StringBuilder(256);
			sqlInsertString.append("insert into ADM_Subscription (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId");
			sqlInsertString.append(", communication_id, receiverIdentifier, declined)");
			sqlInsertString.append(" values (:bizId, :bizVersion, :bizLock, :bizKey, :bizCustomer, :bizUserId");
			sqlInsertString.append(", :communication, :receiverIdentifier, :declined)");

			SQL sqlInsert = persistence.newSQL(sqlInsertString.toString());

			String bizId = UUID.randomUUID().toString();
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
	 * find the subscription public user anonymously
	 * 
	 * If the user has not been created, create it.
	 * 
	 * @param persistence
	 * @param bizCustomer
	 * @return
	 */
	public static boolean anonymouslyCheckSubscriptionPublicUser(Persistence persistence, String bizCustomer) {
		boolean result = false;

		StringBuilder s = new StringBuilder(256);
		s.append("select bizId, userName");
		s.append(" from ADM_SecurityUser");
		s.append(" where bizCustomer = :").append(Bean.CUSTOMER_NAME);
		s.append(" and bizId = :").append(Bean.DOCUMENT_ID);

		SQL sql = persistence.newSQL(s.toString());
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
				StringBuilder sqlInsContactString = new StringBuilder(256);
				sqlInsContactString.append("insert into ADM_Contact (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId");
				sqlInsContactString.append(", name, contactType)");
				sqlInsContactString.append(" values (:bizId, :bizVersion, :bizLock, :bizKey, :bizCustomer, :bizUserId");
				sqlInsContactString.append(", :name, :contactType)");

				SQL sqlInsContact = persistence.newSQL(sqlInsContactString.toString());

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
				StringBuilder sqlInsUserString = new StringBuilder(256);
				sqlInsUserString.append("insert into ADM_SecurityUser (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId");
				sqlInsUserString.append(", userName, contact_id)");
				sqlInsUserString.append(" values (:bizId, :bizVersion, :bizLock, :bizKey, :bizCustomer, :bizUserId");
				sqlInsUserString.append(", :userName, :contact)");

				SQL sqlInsUser = persistence.newSQL(sqlInsUserString.toString());

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
	 * anonymously check whether a subscription exists for a customer, communication and receiver
	 * 
	 * @param p
	 * @param bizCustomer
	 * @param communicationId
	 * @param receiverIdentifier
	 * @return
	 */
	public static boolean anonymouslySubscriptionExists(Persistence p, String bizCustomer, String communicationId, String receiverIdentifier) {

		boolean result = false;

		StringBuilder sqlSubString = new StringBuilder(256);
		sqlSubString.append("select count(*) from ADM_Subscription ");
		sqlSubString.append(" where communication_id = :").append(Subscription.communicationPropertyName);
		sqlSubString.append(" and receiverIdentifier = :").append(Subscription.receiverIdentifierPropertyName);
		sqlSubString.append(" and bizCustomer = :").append(Bean.CUSTOMER_NAME);

		SQL sqlSub = p.newSQL(sqlSubString.toString());
		sqlSub.putParameter(Bean.CUSTOMER_NAME, bizCustomer, false);
		sqlSub.putParameter(Subscription.communicationPropertyName, communicationId, false);
		sqlSub.putParameter(Subscription.receiverIdentifierPropertyName, receiverIdentifier, false);

		// get results
		try {
			BigInteger exists = sqlSub.scalarResult(BigInteger.class);
			result = exists.compareTo(new BigInteger("0"))>0;
		} catch (@SuppressWarnings("unused") DomainException d) {
			// do nothing, return false
		}
		return result;
	}
}