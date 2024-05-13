package modules.test.AllAttributesDynamicPersistent;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedMap;

import org.skyve.domain.Bean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.model.list.InMemoryListModel;

import modules.test.domain.AllAttributesPersistent;

public class TestListModel extends InMemoryListModel<DynamicPersistentBean> {
	private static final List<MetaDataQueryColumn> COLUMNS = new ArrayList<>(1);
	
	static {
		MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(AllAttributesPersistent.colourPropertyName);
		column.setDisplayName("Colour");
		COLUMNS.add(column);
	}
	
	@Override
	public void postConstruct(Customer customer, boolean runtime) {
		Module m = customer.getModule(AllAttributesPersistent.MODULE_NAME);
		setDrivingDocument(m, m.getDocument(customer, AllAttributesPersistent.DOCUMENT_NAME));
		super.postConstruct(customer, runtime);
	}
	
	@Override
	public List<Bean> getRows() throws Exception {
		return new ArrayList<>();
	}

	@Override
	public String getDescription() {
		return "Test";
	}

	@Override
	public List<MetaDataQueryColumn> getColumns() {
		return COLUMNS;
	}

	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
		throw new UnsupportedOperationException();
	}

	@Override
	public void remove(String bizId) throws Exception {
		throw new UnsupportedOperationException();
	}
}
