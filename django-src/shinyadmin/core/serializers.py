from rest_framework import serializers
from .models import DamsMCDAUser, DamsMCDAGroup

class DamsMCDAUserSerializer(serializers.ModelSerializer):
    class Meta:
        model = DamsMCDAUser
        # might have to exclude some sensitive fields below
        exclude = ('password', 'user_permissions',)
