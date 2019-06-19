package modules.admin.User.models;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;

import javax.resource.spi.IllegalStateException;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.model.list.InMemoryListModel;

import modules.admin.User.UserExtension;
import modules.admin.domain.UserRole;

public class AssignedRolesModel extends InMemoryListModel<UserExtension> {
	private static final long serialVersionUID = 7679006026692823224L;

	private List<MetaDataQueryColumn> columns = new ArrayList<>(1);

	public AssignedRolesModel() throws Exception {
        Customer c = CORE.getUser().getCustomer();
        Module m = c.getModule(UserRole.MODULE_NAME);
        Document d = m.getDocument(c, UserRole.DOCUMENT_NAME);
		setDrivingDocument(m, d);

		Set<String> projections = getProjections();
		projections.add(UserRole.roleNamePropertyName);

		MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
		column.setDisplayName("Role Name");
		column.setBinding(UserRole.roleNamePropertyName);
		columns.add(column);
	}

	@Override
	public List<Bean> getRows() throws Exception {
		UserExtension user = getBean();
		if (user == null) {
			return Collections.emptyList();
		}

		return user.getAssignedRoles();
	}

	@Override
	public String getDescription() {
		return "Roles Assigned to the user";
	}

	@Override
	public List<MetaDataQueryColumn> getColumns() {
		return columns;
	}

	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
		throw new IllegalStateException("Not Implemented");
	}

	@Override
	public void remove(String bizId) throws Exception {
		throw new IllegalStateException("Not Implemented");
	}
}
