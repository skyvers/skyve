package modules.admin;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.util.BeanVisitor;
import org.skyve.util.JSON;

/**
 * Serialises an audited bean graph into the JSON structure consumed by audit history.
 *
 * <p>The generator walks only audited documents and audited relations, collecting
 * attribute values into a nested map keyed by binding path so the audit diff view
 * can reconstruct the original payload.
 */
public class AuditJSONGenerator extends BeanVisitor {
	private Map<String, Object> audit = new TreeMap<>();
	private Customer customer;

	/**
	 * Creates a generator for the supplied customer metadata.
	 *
	 * @param customer the customer whose document metadata will be used during traversal; never {@code null}
	 */
	public AuditJSONGenerator(Customer customer) {
		super(false, false);
		this.customer = customer;
	}

	/**
	 * Serialises the collected audit payload to JSON.
	 *
	 * @return the JSON representation of the visited audit tree
	 * @throws Exception if JSON marshalling fails
	 */
	public String toJSON() throws Exception {
		return JSON.marshall(customer, audit);
	}

	/**
	 * Adds the current bean to the audit payload when the document or owning relation is audited.
	 *
	 * <p>Non-audited documents and non-audited relations are skipped so the resulting JSON only
	 * contains values that were explicitly configured for audit capture.
	 *
	 * @param binding the binding path for the current bean; never {@code null}
	 * @param document the current document metadata; never {@code null}
	 * @param owningDocument the owning document metadata, or {@code null} for the root bean
	 * @param owningRelation the relation that led to this bean, or {@code null} for the root bean
	 * @param bean the bean currently being visited; never {@code null}
	 * @return {@code true} when the bean was captured in the audit payload, otherwise {@code false}
	 * @throws Exception if bean traversal or binding extraction fails
	 */
	@Override
	protected boolean accept(String binding,
			Document document,
			Document owningDocument,
			Relation owningRelation,
			Bean bean)
			throws Exception {
		if (!document.isAudited()) {
			return false;
		}
		if ((owningRelation != null) && (!owningRelation.isAudited())) {
			return false;
		}

		Map<String, Object> node = new TreeMap<>();

		node.put(Bean.DOCUMENT_ID, bean.getBizId());

		for (Attribute attribute : document.getAllAttributes(customer)) {
			// Is audited and is not a relation
			if (attribute.isAudited() && (!(attribute instanceof Relation))) {
				String name = attribute.getName();
				node.put(name, BindUtil.get(bean, name));
			}
		}

		if (bean instanceof PersistentBean) {
			node.put(Bean.BIZ_KEY, bean.getBizKey());
		}

		audit.put(binding, node);

		return true;
	}
}
