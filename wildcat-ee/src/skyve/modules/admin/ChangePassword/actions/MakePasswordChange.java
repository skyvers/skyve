package modules.admin.ChangePassword.actions;

import java.security.MessageDigest;
import java.util.List;

import modules.admin.AdminUtil;
import modules.admin.domain.ChangePassword;
import modules.admin.domain.Configuration;
import modules.admin.domain.Configuration.PasswordComplexityModel;
import modules.admin.domain.Contact;

import org.apache.commons.codec.binary.Base64;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.types.DateTime;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.util.BeanValidator;
import org.skyve.web.WebContext;

public class MakePasswordChange implements ServerSideAction<ChangePassword> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4317908281075686229L;

	@Override
	public ServerSideActionResult execute(ChangePassword bean, WebContext webContext) throws Exception {
		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(ChangePassword.MODULE_NAME);
		Document changePasswordDocument = module.getDocument(customer, ChangePassword.DOCUMENT_NAME);

		List<Contact> cs = persistence.newBizQL("select bean from {admin.Contact} as bean").beanResults();
		for (Contact contact : cs) {
			System.out.println(contact);
		}

		Iterable<Contact> i = persistence.newBizQL("select bean from {admin.Contact} as bean").beanIterable();
		for (Contact contact : i) {
			System.out.println(contact);
		}

		Contact c = persistence.newBizQL("select bean as bean from {admin.Contact} as bean").setFirstResult(0).setMaxResults(1).beanResult();
		System.out.println(c);

		List<Bean> bl = persistence.newBizQL("select bean as bean from {admin.Contact} as bean").projectedResults();
		for (Bean b : bl) {
			System.out.println(b);
		}

		Iterable<Bean> bi = persistence.newBizQL("select bean as bean from {admin.Contact} as bean").projectedIterable();
		for (Bean b : bi) {
			System.out.println(b);
		}
		
		Bean sb = persistence.newBizQL("select bean as bean from {admin.Contact} as bean").setFirstResult(0).setMaxResults(1).projectedResult();
		System.out.println(sb);
		
		bl = persistence.newBizQL("select bean.name as name from {admin.Contact} as bean").projectedResults();
		for (Bean b : bl) {
			System.out.println(b);
		}

		bi = persistence.newBizQL("select bean.name as name, bean.bizCustomer as bizCustomer from {admin.Contact} as bean").projectedIterable();
		for (Bean b : bi) {
			System.out.println(b);
		}
		
		sb = persistence.newBizQL("select bean.name as name from {admin.Contact} as bean").setFirstResult(0).setMaxResults(1).projectedResult();
		System.out.println(sb);
		
		List<String> sl = persistence.newBizQL("select bean.name as name from {admin.Contact} as bean").scalarResults(String.class);
		for (String s : sl) {
			System.out.println(s);
		}

		Iterable<String> si = persistence.newBizQL("select bean.name as name from {admin.Contact} as bean").scalarIterable(String.class);
		for (String s : si) {
			System.out.println(s);
		}
		
		System.out.println(persistence.newBizQL("select bean.name from {admin.Contact} as bean").setFirstResult(0).setMaxResults(1).scalarResult(String.class));

		List<Object[]> al = persistence.newBizQL("select bean.name as name, bean.bizCustomer from {admin.Contact} as bean").tupleResults();
		for (Object[] a : al) {
			System.out.println(a);
		}

		Iterable<Object[]> ai = persistence.newBizQL("select bean.name, bean.bizCustomer from {admin.Contact} as bean").tupleIterable();
		for (Object[] a : ai) {
			System.out.println(a);
		}
		
		System.out.println(persistence.newBizQL("select bean.name, bean.bizCustomer from {admin.Contact} as bean").setFirstResult(0).setMaxResults(1).tupleResult());


		
		
		
		
		
		
		
		
		cs = persistence.newDocumentQuery("admin", "Contact").beanResults();
		for (Contact contact : cs) {
			System.out.println(contact);
		}

		i = persistence.newDocumentQuery("admin", "Contact").beanIterable();
		for (Contact contact : i) {
			System.out.println(contact);
		}

		c = persistence.newDocumentQuery("admin", "Contact").setFirstResult(0).setMaxResults(1).beanResult();
		System.out.println(c);

		bl = persistence.newDocumentQuery("admin", "Contact").projectedResults();
		for (Bean b : bl) {
			System.out.println(b);
		}

		bi = persistence.newDocumentQuery("admin", "Contact").projectedIterable();
		for (Bean b : bi) {
			System.out.println(b);
		}
		
		sb = persistence.newDocumentQuery("admin", "Contact").setFirstResult(0).setMaxResults(1).projectedResult();
		System.out.println(sb);
		
		DocumentQuery q = persistence.newDocumentQuery("admin", "Contact");
		q.addBoundProjection("name");
		bl = q.projectedResults();
		for (Bean b : bl) {
			System.out.println(b);
		}

		bi = q.projectedIterable();
		for (Bean b : bi) {
			System.out.println(b);
		}
		
		sb = q.setFirstResult(0).setMaxResults(1).projectedResult();
		System.out.println(sb);
		
		sl = q.scalarResults(String.class);
		for (String s : sl) {
			System.out.println(s);
		}

		si = q.scalarIterable(String.class);
		for (String s : si) {
			System.out.println(s);
		}
		
		System.out.println(q.setFirstResult(0).setMaxResults(1).scalarResult(String.class));

		q.addBoundProjection("bizCustomer");
		
		al = q.tupleResults();
		for (Object[] a : al) {
			System.out.println(a);
		}

		ai = q.tupleIterable();
		for (Object[] a : ai) {
			System.out.println(a);
		}
		
		System.out.println(q.setFirstResult(0).setMaxResults(1).tupleResult());

		
		
		
		
		
		SQL ss = persistence.newSQL("admin", "Contact", "select * from ADM_contact");
		SQL s = persistence.newSQL("admin", "Contact", "select * from ADM_Contact where bizId = 'aa34f239-2e38-4861-9eab-350e1c89ad17'");
		cs = ss.beanResults();
		for (Contact contact : cs) {
			System.out.println(contact);
		}

		i = ss.beanIterable();
		for (Contact contact : i) {
			System.out.println(contact);
		}

		c = s.beanResult();
		System.out.println(c);
/*
		bl = ss.projectedResults();
		for (Bean b : bl) {
			System.out.println(b);
		}

		bi = ss.projectedIterable();
		for (Bean b : bi) {
			System.out.println(b);
		}
		
		sb = s.projectedResult();
		System.out.println(sb);

		ss = persistence.newSQL("select name from ADM_contact");
		s = persistence.newSQL("select name from ADM_Contact where bizId = 'aa34f239-2e38-4861-9eab-350e1c89ad17'");
		bl = ss.projectedResults();
		for (Bean b : bl) {
			System.out.println(b);
		}

		bi = ss.projectedIterable();
		for (Bean b : bi) {
			System.out.println(b);
		}
		
		sb = s.projectedResult();
		System.out.println(sb);
*/		
		sl = ss.scalarResults(String.class);
		for (String str : sl) {
			System.out.println(str);
		}

		si = ss.scalarIterable(String.class);
		for (String str : si) {
			System.out.println(str);
		}
		
		System.out.println(s.scalarResult(String.class));

		ss = persistence.newSQL("select name, bizCustomer from ADM_contact");
		s = persistence.newSQL("select name, bizCustomer from ADM_Contact where bizId = 'aa34f239-2e38-4861-9eab-350e1c89ad17'");
		al = ss.tupleResults();
		for (Object[] a : al) {
			System.out.println(a);
		}

		ai = ss.tupleIterable();
		for (Object[] a : ai) {
			System.out.println(a);
		}
		
		System.out.println(s.tupleResult());

/*		
		BeanValidator.validateBeanAgainstDocument(changePasswordDocument, bean);

		String newPassword = bean.getNewPassword();
		String confirmPassword = bean.getConfirmPassword();

		// check for suitable complexity
		Configuration configuration = AdminUtil.getConfiguration();
		PasswordComplexityModel cm = configuration.getPasswordComplexityModel();

		if (!AdminUtil.validatePasswordComplexity(newPassword, cm)) {
			StringBuilder sb = new StringBuilder("The password you have entered is not sufficiently complex.\n");
			sb.append(AdminUtil.validatePasswordComplexityMessage(cm));
			sb.append("\nPlease re-enter and confirm the password.");

			Message message = new Message(ChangePassword.newPasswordPropertyName, sb.toString());
			throw new ValidationException(message);
		}

		if (! newPassword.equals(confirmPassword)) { // these 2 are mandatory in the document
			Message message = new Message(ChangePassword.newPasswordPropertyName,
					"You did not type the same password.  Please re-enter and confirm the password.");
			message.addBinding(ChangePassword.confirmPasswordPropertyName);
			throw new ValidationException(message);
		}

		Document userDocument = module.getDocument(customer, modules.admin.domain.User.DOCUMENT_NAME);
		modules.admin.domain.User userBean = persistence.retrieve(userDocument, user.getId(), true);

		MessageDigest md = MessageDigest.getInstance(UtilImpl.PASSWORD_HASHING_ALGORITHM);
		Base64 base64Codec = new Base64();
		String hashedPassword = new String(base64Codec.encode(md.digest(newPassword.getBytes())));

		userBean.setPassword(hashedPassword);

		// clear reset password details
		userBean.setPasswordExpired(Boolean.FALSE);
		userBean.setClearTextPassword(null);
		userBean.setPasswordLastChanged(new DateTime());

		userBean = persistence.save(userDocument, userBean);

		// clear out the passwords since the change was successful
		bean.setNewPassword(null);
		bean.setConfirmPassword(null);

		bean.setResponse("Your password has been changed.");
*/
		return new ServerSideActionResult(bean); // stay on the same form
	}
}
