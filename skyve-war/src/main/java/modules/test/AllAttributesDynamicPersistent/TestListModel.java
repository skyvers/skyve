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

/**
 * Provides a minimal in-memory list model used by dynamic-persistent list tests.
 */
public class TestListModel extends InMemoryListModel<DynamicPersistentBean> {
	private static final List<MetaDataQueryColumn> COLUMNS = new ArrayList<>(1);
	
	static {
		MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(AllAttributesPersistent.colourPropertyName);
		column.setDisplayName("Colour");
		COLUMNS.add(column);
	}
	
	/**
	 * Initialises the driving document metadata for this list model.
	 *
	 * @param customer the active customer used for metadata lookup
	 * @param runtime whether the model is being initialised at runtime
	 */
	@Override
	public void postConstruct(Customer customer, boolean runtime) {
		Module m = customer.getModule(AllAttributesPersistent.MODULE_NAME);
		setDrivingDocument(m, m.getDocument(customer, AllAttributesPersistent.DOCUMENT_NAME));
		super.postConstruct(customer, runtime);
	}
	
	/**
	 * Returns the in-memory rows for this test model.
	 *
	 * @return an empty list for deterministic test behaviour
	 * @throws Exception if row resolution fails
	 */
	@Override
	public List<Bean> getRows() throws Exception {
		return new ArrayList<>();
	}

	/**
	 * Returns the display description for this model.
	 *
	 * @return the fixed description string
	 */
	@Override
	public String getDescription() {
		return "Test";
	}

	/**
	 * Returns the projected columns exposed by this list model.
	 *
	 * @return the static projected-column definition list
	 */
	@Override
	public List<MetaDataQueryColumn> getColumns() {
		return COLUMNS;
	}

	/**
	 * Rejects updates because this test model is read-only.
	 *
	 * @param bizId the business id of the row being updated
	 * @param properties the requested property updates
	 * @return never returns normally
	 * @throws Exception always, because updates are unsupported
	 */
	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
		throw new UnsupportedOperationException();
	}

	/**
	 * Rejects deletes because this test model is read-only.
	 *
	 * @param bizId the business id of the row being removed
	 * @throws Exception always, because removal is unsupported
	 */
	@Override
	public void remove(String bizId) throws Exception {
		throw new UnsupportedOperationException();
	}
}
