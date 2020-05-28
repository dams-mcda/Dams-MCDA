from .serializers import RunPreferenceSerializer
from ..models import DamsMCDARunPreference
from rest_framework import viewsets


class RunPreferenceViewSet(viewsets.ModelViewSet):
    """
        A simple ViewSet for viewing and editing
    """
    queryset = DamsMCDARunPreference.objects.all()
    serializer_class = RunPreferenceSerializer

    def get_queryset(self):
        if self.request.method == "GET":
            _user = self.request.GET.get('user', None)
            _group = self.request.GET.get('group', None)
        elif self.request.method == "POST":
            _user = self.request.POST.get('user', None)
            _group = self.request.POST.get('group', None)
        elif self.request.method in ["PATCH", "PUT"]:
            _user = self.request.data.get('user', None)
            _group = self.request.data.get('group', None)

        query = DamsMCDARunPreference.objects.all()

        # query individual results only!
        if _group == "null":
            _group = None
            query = query.filter(group__isnull=True)

        if (_user and _group):
            query = query.filter(user=_user, group=_group)
        elif (_user):
            query = query.filter(user=_user, group__isnull=True)
        elif (_group):
            query = query.filter(group=_group)

        return query


