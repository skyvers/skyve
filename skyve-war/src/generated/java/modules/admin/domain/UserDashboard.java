package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import modules.admin.User.UserExtension;
import modules.admin.UserDashboard.UserDashboardExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * User Dashboard
 * 
 * @navhas n currentUser 0..1 User
 * @navcomposed n favourites 0..n Generic
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class UserDashboard extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "UserDashboard";

	/** @hidden */
	public static final String currentUserPropertyName = "currentUser";

	/** @hidden */
	public static final String favouritesPropertyName = "favourites";

	/**
	 * Current User
	 **/
	private UserExtension currentUser = null;

	/**
	 * Favourites
	 **/
	private List<Generic> favourites = new ChangeTrackingArrayList<>("favourites", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return UserDashboard.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UserDashboard.DOCUMENT_NAME;
	}

	public static UserDashboardExtension newInstance() {
		try {
			return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage("User Dashboard", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof UserDashboard) && 
					this.getBizId().equals(((UserDashboard) o).getBizId()));
	}

	/**
	 * {@link #currentUser} accessor.
	 * @return	The value.
	 **/
	public UserExtension getCurrentUser() {
		return currentUser;
	}

	/**
	 * {@link #currentUser} mutator.
	 * @param currentUser	The new value.
	 **/
	@XmlElement
	public void setCurrentUser(UserExtension currentUser) {
		if (this.currentUser != currentUser) {
			preset(currentUserPropertyName, currentUser);
			this.currentUser = currentUser;
		}
	}

	/**
	 * {@link #favourites} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<Generic> getFavourites() {
		return favourites;
	}

	/**
	 * {@link #favourites} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public Generic getFavouritesElementById(String bizId) {
		return getElementById(favourites, bizId);
	}

	/**
	 * {@link #favourites} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setFavouritesElementById(String bizId, Generic element) {
		setElementById(favourites, element);
	}

	/**
	 * {@link #favourites} add.
	 * @param element	The element to add.
	 **/
	public boolean addFavouritesElement(Generic element) {
		return favourites.add(element);
	}

	/**
	 * {@link #favourites} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addFavouritesElement(int index, Generic element) {
		favourites.add(index, element);
	}

	/**
	 * {@link #favourites} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeFavouritesElement(Generic element) {
		return favourites.remove(element);
	}

	/**
	 * {@link #favourites} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public Generic removeFavouritesElement(int index) {
		return favourites.remove(index);
	}

	/**
	 * True if the logged in user has permission to read jobs
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isCanReadJobs() {
		return (((UserDashboardExtension)this).canReadJobs());
	}

	/**
	 * {@link #isCanReadJobs} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotCanReadJobs() {
		return (! isCanReadJobs());
	}
}
