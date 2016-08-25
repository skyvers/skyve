package org.skyve.util;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.util.ValidationUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

/**
 * 
 */
public class BeanValidator {
	/**
	 * Disallow instantiation
	 */
	private BeanValidator() {
		// nothing to see here
	}
	
	/**
	 * Validate a document instance against its metadata.
	 * 
	 * @param document The document to validate.
	 * @param bean The bean to validate.
	 * @param e The exception to populate.
	 * @throws ValidationException Thrown if validation or access errors are encountered.
	 * @throws MetaDataException	Thrown if the error message cannot access the attributes converter.
	 */
	public static void validateBeanAgainstDocument(Document document, Bean bean) 
	throws ValidationException, MetaDataException {
		ValidationUtil.validateBeanAgainstDocument(document, bean);
	}

	/**
	 * Validate a document instance against its metadata.
	 * 
	 * @param bean The bean to validate.
	 * @param e The exception to populate.
	 * @throws ValidationException Thrown if validation or access errors are encountered.
	 * @throws MetaDataException	When base the base document hierarchy cannot be accessed.
	 */
	public static void validateBeanAgainstDocument(Bean bean) 
	throws ValidationException, MetaDataException {
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		validateBeanAgainstDocument(d, bean);
	}

	/**
	 * Validate a document attribute against its metadata.
	 * 
	 * @param customer	The current customer for this thread
	 * @param attribute The attribute to validate.
	 * @param bean The bean to validate
	 * @param e The exception to populate
	 * @throws ValidationException Only thrown if an exception occurs getting a bean value, 
	 * 								otherwise messages are appended to the e argument.
	 */
	public static void validateBeanPropertyAgainstAttribute(Customer customer, Attribute attribute, Bean bean, ValidationException e)
	throws ValidationException {
		ValidationUtil.validateBeanPropertyAgainstAttribute(customer, attribute, bean, e);
	}

	/**
	 * Validate a document attribute against its metadata.
	 * 
	 * @param attribute The attribute to validate.
	 * @param bean The bean to validate
	 * @param e The exception to populate
	 * @throws ValidationException Only thrown if an exception occurs getting a bean value, 
	 * 								otherwise messages are appended to the e argument.
	 * @throws MetaDataException	When the attribute cannot be validated.
	 */
	public static void validateBeanPropertyAgainstAttribute(Attribute attribute, Bean bean, ValidationException e)
	throws ValidationException, MetaDataException {
		validateBeanPropertyAgainstAttribute(CORE.getUser().getCustomer(), attribute, bean, e);
	}
	
	/**
	 * Validate a document attribute against its metadata.
	 * 
	 * @param attributeName The name of the attribute to validate.
	 * @param bean The bean to validate
	 * @param e The exception to populate
	 * @throws ValidationException Only thrown if an exception occurs getting a bean value, 
	 * 								otherwise messages are appended to the e argument.
	 * @throws MetaDataException	When the attribute cannot be validated.
	 */
	public static void validateBeanPropertyAgainstAttribute(String attributeName, Bean bean, ValidationException e)
	throws ValidationException, MetaDataException {
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		Attribute a = d.getAttribute(attributeName);
		validateBeanPropertyAgainstAttribute(c, a, bean, e);
	}
	
	
	/**
	 * Validate a bean against its bizlet .validate().
	 * NB This validation method does NOT recursively validate using bizlets through
	 * the base document hierarchy as the bizlet class should be arranged in such a way as to extend the
	 * bizlet methods required of the base bizlet classes through the standard java extension mechanism.
	 * 
	 * @param bizlet	To validate against
	 * @param bean	To validate
	 * @throws ValidationException
	 */
	public static <T extends Bean> void validateBeanAgainstBizlet(Bizlet<T> bizlet, T bean) 
	throws ValidationException {
		ValidationUtil.validateBeanAgainstBizlet(bizlet, bean);
	}

	
	/**
	 * Validate a bean against its bizlet .validate().
	 * NB This validation method does NOT recursively validate using bizlets through
	 * the base document hierarchy as the bizlet class should be arranged in such a way as to extend the
	 * bizlet methods required of the base bizlet classes through the standard java extension mechanism.
	 * 
	 * @param document	To get the bizlet to validate against
	 * @param bean	To validate
	 * @throws ValidationException
	 * @throws MetaDataException	when the Bizlet cannot be obtained
	 */
	public static <T extends Bean> void validateBeanAgainstBizlet(Document document, T bean) 
	throws ValidationException, MetaDataException {
		Bizlet<T> bizlet = ((DocumentImpl) document).getBizlet(CORE.getUser().getCustomer());
		if (bizlet != null) { // has a bizlet
			validateBeanAgainstBizlet(bizlet, bean);
		}
	}
	
	/**
	 * Validate a bean against its bizlet .validate().
	 * NB This validation method does NOT recursively validate using bizlets through
	 * the base document hierarchy as the bizlet class should be arranged in such a way as to extend the
	 * bizlet methods required of the base bizlet classes through the standard java extension mechanism.
	 * 
	 * @param bean	To validate
	 * @throws ValidationException
	 * @throws MetaDataException	when the Bizlet cannot be obtained
	 */
	public static <T extends Bean> void validateBeanAgainstBizlet(T bean) 
	throws ValidationException, MetaDataException {
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		validateBeanAgainstBizlet(d, bean);
	}

	/**
	 * Updates the bindings in the error message (and its children) 
	 * based on the binding of the validatedBean in relation to the masterBean.
	 * 
	 * @param customer
	 * @param e
	 * @param masterBean
	 * @param validatedBean
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public static void processErrorMessageBindings(Customer customer,
													final Message e,
													Bean masterBean,
													final Bean validatedBean) 
	throws DomainException, MetaDataException {
		ValidationUtil.processErrorMessageBindings(customer, e, masterBean, validatedBean);
	}

	/**
	 * 
	 * @param customer
	 * @param document
	 * @param bean
	 * @throws UniqueConstraintViolationException
	 * @throws ValidationException
	 */
	public static void checkCollectionUniqueConstraints(Customer customer, Document document, Bean bean)
	throws UniqueConstraintViolationException, ValidationException {
		ValidationUtil.checkCollectionUniqueConstraints(customer, document, bean);
	}
	
	/**
	 * 
	 * @param customer
	 * @param bean
	 * @throws UniqueConstraintViolationException
	 * @throws ValidationException
	 * @throws MetaDataException	When the unique constraints cannot be checked.
	 */
	public static void checkCollectionUniqueConstraints(Customer customer, Bean bean)
	throws UniqueConstraintViolationException, ValidationException, MetaDataException {
		Module m = customer.getModule(bean.getBizModule());
		Document d = m.getDocument(customer, bean.getBizDocument());
		checkCollectionUniqueConstraints(customer, d, bean);
	}

	/**
	 * 
	 * @param document
	 * @param bean
	 * @throws UniqueConstraintViolationException
	 * @throws ValidationException
	 * @throws MetaDataException	When the unique constraints cannot be checked.
	 */
	public static void checkCollectionUniqueConstraints(Document document, Bean bean)
	throws UniqueConstraintViolationException, ValidationException, MetaDataException {
		checkCollectionUniqueConstraints(CORE.getUser().getCustomer(), document, bean);
	}
	
	/**
	 * 
	 * @param document
	 * @param bean
	 * @throws UniqueConstraintViolationException
	 * @throws ValidationException
	 * @throws MetaDataException	When the unique constraints cannot be checked.
	 */
	public static void checkCollectionUniqueConstraints(Bean bean)
	throws UniqueConstraintViolationException, ValidationException, MetaDataException {
		checkCollectionUniqueConstraints(CORE.getUser().getCustomer(), bean);
	}

}
