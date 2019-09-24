from rest_framework.routers import DefaultRouter
from .views import RunPreferenceViewSet

router = DefaultRouter()
router.register(r'preference', RunPreferenceViewSet)
urlpatterns = router.urls
