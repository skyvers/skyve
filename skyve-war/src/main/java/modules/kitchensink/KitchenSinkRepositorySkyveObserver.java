package modules.kitchensink;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.repository.LockableDynamicRepository;
import org.skyve.metadata.controller.Observer;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

import jakarta.servlet.http.HttpSession;

public class KitchenSinkRepositorySkyveObserver implements Observer {
	public static final LockableDynamicRepository KITCHEN_SINK_REPOSITORY = new LockableDynamicRepository();

	@Override
	public void startup(Customer customer) {
		DefaultRepository r = (DefaultRepository) CORE.getRepository();
		r.addDelegate(0, KITCHEN_SINK_REPOSITORY);

	}

	@Override
	public void shutdown(Customer customer) {
		// TODO Auto-generated method stub

	}

	@Override
	public void beforeRestore(Customer customer) {
		// TODO Auto-generated method stub

	}

	@Override
	public void afterRestore(Customer customer) {
		// TODO Auto-generated method stub

	}

	@Override
	public void login(User user, HttpSession session) {
		// TODO Auto-generated method stub

	}

	@Override
	public void logout(User user, HttpSession session) {
		// TODO Auto-generated method stub

	}

	@Override
	public void beforeBackup(Customer customer) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void afterBackup(Customer customer) {
		// TODO Auto-generated method stub
		
	}
}
