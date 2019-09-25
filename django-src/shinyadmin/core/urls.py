from django.urls import path, include
from .api import urls as api_urls

urlpatterns = [
    # user login
    path('api/', include(api_urls), name="api"),
]
