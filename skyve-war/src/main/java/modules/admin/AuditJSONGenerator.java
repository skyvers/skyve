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

public class AuditJSONGenerator extends BeanVisitor {
	private Map<String, Object> audit = new TreeMap<>();
	private Customer customer;
	
	public AuditJSONGenerator(Customer customer) {
		super(false, false, false);
		this.customer = customer;
	}
	
	public String toJSON() throws Exception {
		return JSON.marshall(customer, audit);
	}

	@Override
	protected boolean accept(String binding,
								Document document,
								Document owningDocument,
								Relation owningRelation,
								Bean bean)
	throws Exception {
		if (! document.isAudited()) {
			return false;
		}
		if ((owningRelation != null) && (! owningRelation.isAudited())) {
			return false;
		}

		Map<String, Object> node = new TreeMap<>();

		node.put(Bean.DOCUMENT_ID, bean.getBizId());
		
		for (Attribute attribute : document.getAllAttributes(customer)) {
			// Is audited and is not a relation
			if (attribute.isAudited() && (! (attribute instanceof Relation))) {
				String name = attribute.getName();
				node.put(name, BindUtil.get(bean, name));
			}
		}

		if (bean instanceof PersistentBean) {
			node.put(Bean.BIZ_KEY, ((PersistentBean) bean).getBizKey());
		}
		
		audit.put(binding, node);
		
		return true;
	}
}
